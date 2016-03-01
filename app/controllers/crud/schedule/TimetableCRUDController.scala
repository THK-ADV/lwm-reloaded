package controllers.crud.schedule

import java.util.UUID

import controllers.crud.AbstractCRUDController
import models.users.Employee
import models.{Degree, Room, Labwork, UriGenerator}
import models.schedule._
import models.security.Permissions._
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Json, JsValue, Reads, Writes}
import services.RoleService
import store.{Namespace, SesameRepository}
import utils.LwmMimeType

import scala.collection.Map
import scala.util.{Success, Try}

class TimetableCRUDController(val repository: SesameRepository, val namespace: Namespace, val roleService: RoleService) extends AbstractCRUDController[TimetableProtocol, Timetable] {

  override implicit def reads: Reads[TimetableProtocol] = Timetable.reads

  override implicit def writes: Writes[Timetable] = Timetable.writes

  override implicit val mimeType: LwmMimeType = LwmMimeType.timetableV1Json

  override implicit def rdfReads: FromPG[Sesame, Timetable] = defaultBindings.TimetableBinding.timetableBinder

  override implicit def classUrisFor: ClassUrisFor[Sesame, Timetable] = defaultBindings.TimetableBinding.classUri

  override implicit def uriGenerator: UriGenerator[Timetable] = Timetable

  override implicit def rdfWrites: ToPG[Sesame, Timetable] = defaultBindings.TimetableBinding.timetableBinder

  override protected def fromInput(input: TimetableProtocol, id: Option[UUID]): Timetable = id match {
    case Some(uuid) => Timetable(input.labwork, input.entries, input.start, input.localBlacklist, uuid)
    case None => Timetable(input.labwork, input.entries, input.start, input.localBlacklist, Timetable.randomUUID)
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case _ => PartialSecureBlock(Set(prime))
  }

  override protected def restrictedContext(moduleId: String): PartialFunction[Rule, SecureContext] = {
    case Create => SecureBlock(moduleId, Set(createTimetable))
    case All => SecureBlock(moduleId, Set(allTimetables))
    case Update => SecureBlock(moduleId, Set(updateTimetable))
    case Get => SecureBlock(moduleId, Set(getTimetable))
    case Delete => SecureBlock(moduleId, Set(deleteTimetable))
    case _ => PartialSecureBlock(Set(prime))
  }

  def createFrom(labwork: String) = restrictedContext(labwork)(Create) asyncContentTypedAction { request =>
    super.create(NonSecureBlock)(request)
  }

  def allFrom(labwork: String) = restrictedContext(labwork)(All) asyncAction { request =>
    super.all(NonSecureBlock)(request)
  }

  def updateFrom(labwork: String, id: String) = restrictedContext(labwork)(Update) asyncContentTypedAction { request =>
    super.update(id, NonSecureBlock)(request)
  }

  def getFrom(labwork: String, id: String) = restrictedContext(labwork)(Get) asyncAction { request =>
    super.get(id, NonSecureBlock)(request)
  }

  def deleteFrom(labwork: String, id: String) = restrictedContext(labwork)(Delete) asyncAction { request =>
    super.delete(id, NonSecureBlock)(request)
  }

  override protected def compareModel(input: TimetableProtocol, output: Timetable): Boolean = {
    input.labwork == output.labwork && input.start == output.start && input.localBlacklist == output.localBlacklist && input.entries == output.entries
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
      supervisors <- repository.getMany[Employee](output.entries.map(e => Employee.generateUri(e.supervisor)(namespace)))
      degrees <- repository.getMany[Degree](output.entries.map(e => Degree.generateUri(e.degree)(namespace)))
    } yield {
      labwork.map { l =>
        val entries = output.entries.foldLeft(Set.empty[TimetableEntryAtom]) { (newSet, e) =>
          (for {
            r <- rooms.find(_.id == e.room)
            s <- supervisors.find(_.id == e.supervisor)
            d <- degrees.find(_.id == e.degree)
          } yield TimetableEntryAtom(s, r, d, e.dayIndex, e.start, e.end, e.id)) match {
            case Some(atom) => newSet + atom
            case None => newSet
          }
        }
        Json.toJson(TimetableAtom(l, entries, output.start, output.localBlacklist, output.id))(Timetable.atomicWrites)
      }
    }
  }

  override protected def atomizeMany(output: Set[Timetable]): Try[JsValue] = {
    import defaultBindings.LabworkBinding._
    import defaultBindings.RoomBinding._
    import defaultBindings.EmployeeBinding._
    import defaultBindings.DegreeBinding._
    import Timetable._
    import TimetableEntry._
    import utils.Ops._
    import utils.Ops.MonadInstances.tryM

    (for {
      labworks <- repository.getMany[Labwork](output.map(s => Labwork.generateUri(s.labwork)(namespace)))
      rooms <- output.map(s => repository.getMany[Room](s.entries.map(e => Room.generateUri(e.room)(namespace)))).sequence
      supervisors <- output.map(s => repository.getMany[Employee](s.entries.map(e => Employee.generateUri(e.supervisor)(namespace)))).sequence
      degrees <- output.map(s => repository.getMany[Degree](s.entries.map(e => Degree.generateUri(e.degree)(namespace)))).sequence
    } yield {
      output.foldLeft(Set.empty[TimetableAtom]) { (set, t) =>
        labworks.find(_.id == t.labwork) match {
          case Some(l) =>
            val entries = t.entries.foldLeft(Set.empty[TimetableEntryAtom]) { (setE, e) =>
              (for {
                r <- rooms.flatten.find(_.id == e.room)
                s <- supervisors.flatten.find(_.id == e.supervisor)
                d <- degrees.flatten.find(_.id == e.degree)
              } yield TimetableEntryAtom(s, r, d, e.dayIndex, e.start, e.end, e.id)) match {
                case Some(entryAtom) => setE + entryAtom
                case None => setE
              }
            }

            val atom = TimetableAtom(l, entries, t.start, t.localBlacklist, t.id)
            set + atom
          case None => set
        }
      }
    }).map(s => Json.toJson(s)(Timetable.setAtomicWrites))

  }
}