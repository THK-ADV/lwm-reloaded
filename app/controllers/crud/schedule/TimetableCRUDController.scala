package controllers.crud.schedule

import java.util.UUID
import controllers.crud.AbstractCRUDController
import models.semester.{BlacklistProtocol, Blacklist}
import models.users.{User, Employee}
import models.{Degree, Room, Labwork, UriGenerator}
import models.schedule._
import models.security.Permissions._
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{JsValue, Json, Reads, Writes}
import services.{RoleService, SessionHandlingService}
import store.{Namespace, SesameRepository}
import utils.LwmMimeType

import scala.collection.Map
import scala.util.{Success, Try}

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
      Timetable(input.labwork, input.entries, input.start, Blacklist(input.localBlacklist.dates, timetable.localBlacklist.id), timetable.id)
    case None =>
      Timetable(input.labwork, input.entries, input.start, Blacklist(input.localBlacklist.dates, Blacklist.randomUUID), Timetable.randomUUID)
  }

  override protected def compareModel(input: TimetableProtocol, output: Timetable): Boolean = {
    input.start == output.start && input.localBlacklist == BlacklistProtocol(output.localBlacklist.dates) && input.entries == output.entries
  }

  override protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[Timetable]): Try[Set[Timetable]] = Success(all)

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
    } yield {
      labwork.map { l =>
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
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case Create => SecureBlock(restrictionId, timetable.create)
    case Get => SecureBlock(restrictionId, timetable.get)
    case GetAll => SecureBlock(restrictionId, timetable.getAll)
    case Update => SecureBlock(restrictionId, timetable.update)
    case Delete => SecureBlock(restrictionId, timetable.delete)
  }

  def createFrom(course: String) = restrictedContext(course)(Create) asyncContentTypedAction { request =>
    val newRequest = AbstractCRUDController.rebaseUri(request, Timetable.generateBase)
    super.create(NonSecureBlock)(newRequest)
  }

  def updateFrom(course: String, timetable: String) = restrictedContext(course)(Update) asyncContentTypedAction { request =>
    val newRequest = AbstractCRUDController.rebaseUri(request, Timetable.generateBase(UUID.fromString(timetable)))
    super.update(timetable, NonSecureBlock)(newRequest)
  }

  def createAtomicFrom(course: String) = restrictedContext(course)(Create) asyncContentTypedAction { request =>
    val newRequest = AbstractCRUDController.rebaseUri(request, Timetable.generateBase)
    super.createAtomic(NonSecureBlock)(newRequest)
  }

  def updateAtomicFrom(course: String, timetable: String) = restrictedContext(course)(Update) asyncContentTypedAction { request =>
    val newRequest = AbstractCRUDController.rebaseUri(request, Timetable.generateBase(UUID.fromString(timetable)))
    super.updateAtomic(timetable, NonSecureBlock)(newRequest)
  }

  def allFrom(course: String) = restrictedContext(course)(GetAll) asyncAction { request =>
    val newRequest = AbstractCRUDController.rebaseUri(request, Timetable.generateBase)
    super.all(NonSecureBlock)(newRequest)
  }

  def allAtomicFrom(course: String) = restrictedContext(course)(GetAll) asyncAction { request =>
    val newRequest = AbstractCRUDController.rebaseUri(request, Timetable.generateBase)
    super.allAtomic(NonSecureBlock)(newRequest)
  }

  def getFrom(course: String, timetable: String) = restrictedContext(course)(Get) asyncAction { request =>
    val newRequest = AbstractCRUDController.rebaseUri(request, Timetable.generateBase(UUID.fromString(timetable)))
    super.get(timetable, NonSecureBlock)(newRequest)
  }

  def getAtomicFrom(course: String, timetable: String) = restrictedContext(course)(Get) asyncAction { request =>
    val newRequest = AbstractCRUDController.rebaseUri(request, Timetable.generateBase(UUID.fromString(timetable)))
    super.getAtomic(timetable, NonSecureBlock)(newRequest)
  }

  def deleteFrom(course: String, timetable: String) = restrictedContext(course)(Delete) asyncAction { request =>
    val newRequest = AbstractCRUDController.rebaseUri(request, Timetable.generateBase(UUID.fromString(timetable)))
    super.delete(timetable, NonSecureBlock)(newRequest)
  }
}