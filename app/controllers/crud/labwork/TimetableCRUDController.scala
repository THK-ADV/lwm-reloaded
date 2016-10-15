package controllers.crud.labwork

import java.util.UUID

import controllers.crud.AbstractCRUDController
import controllers.crud.labwork.TimetableCRUDController._
import models._
import models.labwork._
import models.security.Permissions._
import org.openrdf.model.Value
import org.w3.banana.RDFPrefix
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Reads, Writes}
import services.{RoleService, SessionHandlingService}
import store.Prefixes.LWMPrefix
import store.bind.Descriptor.Descriptor
import store.{Namespace, SesameRepository}
import utils.LwmMimeType

import scala.collection.Map
import scala.util.{Failure, Try}

object TimetableCRUDController {
  val courseAttribute = "course"
  val labworkAttribute = "labwork"
}

class TimetableCRUDController(val repository: SesameRepository, val sessionService: SessionHandlingService, implicit val namespace: Namespace, val roleService: RoleService) extends AbstractCRUDController[TimetableProtocol, Timetable, TimetableAtom] {

  override implicit val mimeType: LwmMimeType = LwmMimeType.timetableV1Json

  override implicit val descriptor: Descriptor[Sesame, Timetable] = defaultBindings.TimetableDescriptor

  override implicit val descriptorAtom: Descriptor[Sesame, TimetableAtom] = defaultBindings.TimetableAtomDescriptor

  override implicit val reads: Reads[TimetableProtocol] = Timetable.reads

  override implicit val writes: Writes[Timetable] = Timetable.writes

  override implicit val writesAtom: Writes[TimetableAtom] = Timetable.writesAtom

  override implicit val uriGenerator: UriGenerator[Timetable] = Timetable

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

  override protected def coAtomic(atom: TimetableAtom): Timetable = Timetable(
    atom.labwork.id,
    atom.entries map (te => TimetableEntry(te.supervisor map (_.id), te.room.id, te.dayIndex, te.start, te.end)),
    atom.start, atom.localBlacklist, atom.invalidated, atom.id
  )

  /*
  override protected def compareModel(input: TimetableProtocol, output: Timetable): Boolean = {
    import models.semester.Blacklist.dateOrd

    input.start.isEqual(output.start) &&
      //input.entries == output.entries &&
      input.entries.zip(output.entries ).forall(e => e._1.equals(e._2)) &&
      (input.localBlacklist.size == output.localBlacklist.size || input.localBlacklist.toVector.sorted.zip(output.localBlacklist.toVector.sorted).forall(d => d._1.isEqual(d._2)))
      //input.localBlacklist.toVector.sorted.zip(output.localBlacklist.toVector.sorted).forall(d => d._1.isEqual(d._2))

  }
  */

  override protected def compareModel(input: TimetableProtocol, output: Timetable): Boolean = {
    input.start.isEqual(output.start) &&
      input.entries == output.entries &&
      input.localBlacklist.diff(output.localBlacklist).isEmpty
  }

  override protected def fromInput(input: TimetableProtocol, existing: Option[Timetable]): Timetable = existing match {
    case Some(timetable) =>
      Timetable(input.labwork, input.entries, input.start, input.localBlacklist, timetable.invalidated, timetable.id)
    case None =>
      Timetable(input.labwork, input.entries, input.start, input.localBlacklist)
  }

  override protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[Timetable]): Try[Set[Timetable]] = {
    import store.sparql.select
    import store.sparql.select._
    import utils.Ops.MonadInstances.listM
    import defaultBindings.LabworkDescriptor

    lazy val lwm = LWMPrefix[repository.Rdf]
    lazy val rdf = RDFPrefix[repository.Rdf]

    queryString.foldRight(Try[Set[Timetable]](all)) {
      case ((`labworkAttribute`, labworks), timetables) =>
        timetables flatMap (set => Try(UUID.fromString(labworks.head)) map (id => set.filter(_.labwork == id)))
      case ((`courseAttribute`, courses), timetables) =>
        val query = select("labworks") where {
          **(v("labworks"), p(rdf.`type`), s(lwm.Labwork)).
            **(v("labworks"), p(lwm.course), s(Course.generateUri(UUID.fromString(courses.head))))
        }

        repository.prepareQuery(query).
          select(_.get("labworks")).
          transform(_.fold(List.empty[Value])(identity)).
          map(_.stringValue).
          requestAll(repository.getMany[Labwork]).
          requestAll[Set, Timetable](labworks => timetables.map(_.filter(tt => labworks.exists(_.id == tt.labwork)))).
          run
      case ((_, _), set) => Failure(new Throwable("Unknown attribute"))
    }
  }
}