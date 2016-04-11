package controllers.crud.labwork

import java.util.UUID
import controllers.crud.AbstractCRUDController
import models.labwork._
import models.users.{User, Employee}
import models._
import models.security.Permissions._
import org.openrdf.model.Value
import org.w3.banana.RDFPrefix
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{JsValue, Json, Reads, Writes}
import services.{RoleService, SessionHandlingService}
import store.Prefixes.LWMPrefix
import store.{Namespace, SesameRepository}
import utils.LwmMimeType
import utils.RequestOps._
import scala.collection.Map
import scala.util.{Failure, Try}
import TimetableCRUDController._

object TimetableCRUDController {
  val courseAttribute = "course"
}

class TimetableCRUDController(val repository: SesameRepository, val sessionService: SessionHandlingService, val namespace: Namespace, val roleService: RoleService) extends AbstractCRUDController[TimetableProtocol, Timetable] {

  override implicit def reads: Reads[TimetableProtocol] = Timetable.reads

  override implicit def writes: Writes[Timetable] = Timetable.writes

  override implicit val mimeType: LwmMimeType = LwmMimeType.timetableV1Json

  override implicit def rdfReads: FromPG[Sesame, Timetable] = defaultBindings.TimetableBinding.timetableBinder

  override implicit def classUrisFor: ClassUrisFor[Sesame, Timetable] = defaultBindings.TimetableBinding.classUri

  override implicit def uriGenerator: UriGenerator[Timetable] = Timetable

  override implicit def rdfWrites: ToPG[Sesame, Timetable] = defaultBindings.TimetableBinding.timetableBinder

  override protected def fromInput(input: TimetableProtocol, existing: Option[Timetable]): Timetable = existing match {
    case Some(timetable) =>
      Timetable(input.labwork, input.entries, input.start, input.localBlacklist, timetable.id)
    case None =>
      Timetable(input.labwork, input.entries, input.start, input.localBlacklist, Timetable.randomUUID)
   }


  override protected def compareModel(input: TimetableProtocol, output: Timetable): Boolean = {
    import models.semester.Blacklist.dateOrd

    input.start == output.start &&
      input.entries == output.entries &&
      input.localBlacklist.toVector.sorted.zip(output.localBlacklist.toVector.sorted).forall(d => d._1.isEqual(d._2))
  }

  override protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[Timetable]): Try[Set[Timetable]] = {
    import defaultBindings.LabworkBinding.labworkBinder
    import utils.Ops.MonadInstances.listM
    import store.sparql.select
    import store.sparql.select._
    lazy val lwm = LWMPrefix[repository.Rdf]
    lazy val rdf = RDFPrefix[repository.Rdf]

    queryString.foldRight(Try[Set[Timetable]](all)) {
      case ((`courseAttribute`, values), t) =>
        val query = select ("labworks") where {
          ^(v("labworks"), p(rdf.`type`), s(lwm.Labwork)).
            ^(v("labworks"), p(lwm.course), s(Course.generateUri(UUID.fromString(values.head))(namespace)))
        }

        repository.prepareQuery(query).
          select(_.get("labworks")).
          transform(_.fold(List.empty[Value])(identity)).
          map(_.stringValue).
          requestAll(repository.getMany[Labwork](_)).
          requestAll[Set, Timetable](labworks => t.map(_.filter(tt => labworks.exists(_.id == tt.labwork)))).
          run
      case ((_, _), set) => Failure(new Throwable("Unknown attribute"))
    }
  }

  override protected def atomize(output: Timetable): Try[Option[JsValue]] = {
    import defaultBindings.LabworkBinding._
    import defaultBindings.RoomBinding._
    import defaultBindings.EmployeeBinding._
    import defaultBindings.DegreeBinding._
    import Timetable._
    import TimetableEntry._

    for {
      labwork <- repository.get[Labwork](Labwork.generateUri(output.labwork)(namespace))
      rooms <- repository.getMany[Room](output.entries.map(e => Room.generateUri(e.room)(namespace)))
      supervisors <- repository.getMany[Employee](output.entries.map(e => User.generateUri(e.supervisor)(namespace)))
      degrees <- repository.getMany[Degree](output.entries.map(e => Degree.generateUri(e.degree)(namespace)))
    } yield labwork.map { l =>
      val entries = output.entries.foldLeft(Set.empty[TimetableEntryAtom]) { (newSet, e) =>
        (for {
          r <- rooms.find(_.id == e.room)
          s <- supervisors.find(_.id == e.supervisor)
          d <- degrees.find(_.id == e.degree)
        } yield TimetableEntryAtom(s, r, d, e.dayIndex, e.start, e.end)) match {
          case Some(atom) => newSet + atom
          case None => newSet
        }
      }
      Json.toJson(TimetableAtom(l, entries, output.start, output.localBlacklist, output.id))(Timetable.atomicWrites)
    }
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case Create => SecureBlock(restrictionId, timetable.create)
    case Get => SecureBlock(restrictionId, timetable.get)
    case GetAll => SecureBlock(restrictionId, timetable.getAll)
    case Update => SecureBlock(restrictionId, timetable.update)
    case Delete => SecureBlock(restrictionId, timetable.delete)
  }

  def createFrom(course: String, labwork: String) = restrictedContext(course)(Create) asyncContentTypedAction { implicit request =>
    create(NonSecureBlock)(rebase(Timetable.generateBase))
  }

  def updateFrom(course: String, labwork: String, timetable: String) = restrictedContext(course)(Update) asyncContentTypedAction { implicit request =>
    update(timetable, NonSecureBlock)(rebase(Timetable.generateBase(UUID.fromString(timetable))))
  }

  def createAtomicFrom(course: String, labwork: String) = restrictedContext(course)(Create) asyncContentTypedAction { implicit request =>
    createAtomic(NonSecureBlock)(rebase(Timetable.generateBase))
  }

  def updateAtomicFrom(course: String, labwork: String, timetable: String) = restrictedContext(course)(Update) asyncContentTypedAction { implicit request =>
    updateAtomic(timetable, NonSecureBlock)(rebase(Timetable.generateBase(UUID.fromString(timetable))))
  }

  def allFrom(course: String, labwork: String) = restrictedContext(course)(GetAll) asyncAction { implicit request =>
    all(NonSecureBlock)(rebase(Timetable.generateBase, courseAttribute -> Seq(course)))
  }

  def allAtomicFrom(course: String, labwork: String) = restrictedContext(course)(GetAll) asyncAction { implicit request =>
    allAtomic(NonSecureBlock)(rebase(Timetable.generateBase, courseAttribute -> Seq(course)))
  }

  def getFrom(course: String, labwork: String, timetable: String) = restrictedContext(course)(Get) asyncAction { implicit request =>
    get(timetable, NonSecureBlock)(rebase(Timetable.generateBase(UUID.fromString(timetable))))
  }

  def getAtomicFrom(course: String, labwork: String, timetable: String) = restrictedContext(course)(Get) asyncAction { implicit request =>
    getAtomic(timetable, NonSecureBlock)(rebase(Timetable.generateBase(UUID.fromString(timetable))))
  }

  def deleteFrom(course: String, labwork: String, timetable: String) = restrictedContext(course)(Delete) asyncAction { implicit request =>
    delete(timetable, NonSecureBlock)(rebase(Timetable.generateBase(UUID.fromString(timetable))))
  }
}