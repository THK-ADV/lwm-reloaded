package controllers

import java.util.UUID

import models.Permissions._
import models._
import org.openrdf.model.Value
import org.w3.banana.RDFPrefix
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Reads, Writes}
import services.{RoleServiceLike, SessionHandlingService}
import store.Prefixes.LWMPrefix
import store.bind.Descriptor.Descriptor
import store.{Namespace, SesameRepository}
import utils.LwmMimeType
import controllers.TimetableCRUDController._

import scala.util.{Failure, Try}

object TimetableCRUDController {
  val courseAttribute = "course"
  val labworkAttribute = "labwork"
}

class TimetableCRUDController(val repository: SesameRepository, val sessionService: SessionHandlingService, implicit val namespace: Namespace, val roleService: RoleServiceLike) extends AbstractCRUDController[SesameTimetableProtocol, SesameTimetable, SesameTimetableAtom] {

  override implicit val mimeType: LwmMimeType = LwmMimeType.timetableV1Json

  override implicit val descriptor: Descriptor[Sesame, SesameTimetable] = defaultBindings.TimetableDescriptor

  override implicit val descriptorAtom: Descriptor[Sesame, SesameTimetableAtom] = defaultBindings.TimetableAtomDescriptor

  override implicit val reads: Reads[SesameTimetableProtocol] = SesameTimetable.reads

  override implicit val writes: Writes[SesameTimetable] = SesameTimetable.writes

  override implicit val writesAtom: Writes[SesameTimetableAtom] = SesameTimetable.writesAtom

  override implicit val uriGenerator: UriGenerator[SesameTimetable] = SesameTimetable

  def createFrom(course: String) = restrictedContext(course)(Create) asyncContentTypedAction { implicit request =>
    create(NonSecureBlock)(rebase)
  }

  def createAtomicFrom(course: String) = restrictedContext(course)(Create) asyncContentTypedAction { implicit request =>
    createAtomic(NonSecureBlock)(rebase)
  }

  def updateFrom(course: String, timetable: String) = restrictedContext(course)(Update) asyncContentTypedAction { implicit request =>
    update(timetable, NonSecureBlock)(rebase(timetable))
  }

  def updateAtomicFrom(course: String, timetable: String) = restrictedContext(course)(Update) asyncContentTypedAction { implicit request =>
    updateAtomic(timetable, NonSecureBlock)(rebase(timetable))
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case Create => SecureBlock(restrictionId, timetable.create)
    case Get => SecureBlock(restrictionId, timetable.get)
    case GetAll => SecureBlock(restrictionId, timetable.getAll)
    case Update => SecureBlock(restrictionId, timetable.update)
    case Delete => SecureBlock(restrictionId, timetable.delete)
  }

  def allFrom(course: String) = restrictedContext(course)(GetAll) asyncAction { implicit request =>
    all(NonSecureBlock)(rebase(courseAttribute -> Seq(course)))
  }

  def allAtomicFrom(course: String) = restrictedContext(course)(GetAll) asyncAction { implicit request =>
    allAtomic(NonSecureBlock)(rebase(courseAttribute -> Seq(course)))
  }

  def getFrom(course: String, timetable: String) = restrictedContext(course)(Get) asyncAction { implicit request =>
    get(timetable, NonSecureBlock)(rebase(timetable))
  }

  def getAtomicFrom(course: String, timetable: String) = restrictedContext(course)(Get) asyncAction { implicit request =>
    getAtomic(timetable, NonSecureBlock)(rebase(timetable))
  }

  def deleteFrom(course: String, timetable: String) = restrictedContext(course)(Delete) asyncAction { implicit request =>
    delete(timetable, NonSecureBlock)(rebase(timetable))
  }

  override protected def coAtomic(atom: SesameTimetableAtom): SesameTimetable = SesameTimetable(
    atom.labwork.id,
    atom.entries map (te => SesameTimetableEntry(te.supervisor map (_.id), te.room.id, te.dayIndex, te.start, te.end)),
    atom.start, atom.localBlacklist, atom.invalidated, atom.id
  )

  override protected def compareModel(input: SesameTimetableProtocol, output: SesameTimetable): Boolean = {
    input.start.isEqual(output.start) &&
      input.entries == output.entries &&
      LwmDateTime.isEqual(input.localBlacklist, output.localBlacklist)
  }

  override protected def fromInput(input: SesameTimetableProtocol, existing: Option[SesameTimetable]): SesameTimetable = existing match {
    case Some(timetable) =>
      SesameTimetable(input.labwork, input.entries, input.start, input.localBlacklist.map(LwmDateTime.toDateTime), timetable.invalidated, timetable.id)
    case None =>
      SesameTimetable(input.labwork, input.entries, input.start, input.localBlacklist.map(LwmDateTime.toDateTime))
  }

  override protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[SesameTimetable]): Try[Set[SesameTimetable]] = {
    import defaultBindings.LabworkDescriptor
    import store.sparql.select
    import store.sparql.select._
    import utils.Ops.MonadInstances.listM

    lazy val lwm = LWMPrefix[repository.Rdf]
    lazy val rdf = RDFPrefix[repository.Rdf]

    queryString.foldRight(Try[Set[SesameTimetable]](all)) {
      case ((`labworkAttribute`, labworks), timetables) =>
        timetables flatMap (set => Try(UUID.fromString(labworks.head)) map (id => set.filter(_.labwork == id)))
      case ((`courseAttribute`, courses), timetables) =>
        val query = select("labworks") where {
          **(v("labworks"), p(rdf.`type`), s(lwm.Labwork)).
            **(v("labworks"), p(lwm.course), s(SesameCourse.generateUri(UUID.fromString(courses.head))))
        }

        repository.prepareQuery(query).
          select(_.get("labworks")).
          transform(_.fold(List.empty[Value])(identity)).
          map(_.stringValue).
          requestAll(repository.getMany[SesameLabwork]).
          requestAll[Set, SesameTimetable](labworks => timetables.map(_.filter(tt => labworks.exists(_.id == tt.labwork)))).
          run
      case ((_, _), set) => Failure(new Throwable("Unknown attribute"))
    }
  }
}