package controllers.crud.schedule

import java.util.UUID

import controllers.crud.AbstractCRUDController
import models.UriGenerator
import models.schedule.{Timetable, TimetableProtocol}
import models.security.Permissions._
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.Result
import services.RoleService
import store.{Namespace, SesameRepository}
import utils.LwmMimeType

import scala.collection.Map

// TODO do we actual need this? flow in ScheduleCRUDController would be like: frontend -> timetableProtocol -> timetableService.applyStuff(timetableProtocol): Timetable -> scheduleService.populate(...)
// TODO i guess we will never produces and maintain any timetables without schedules. they are only used in combination with schedules
// TODO consider get and delete, remove create and update
class TimetableCRUDController(val repository: SesameRepository, val namespace: Namespace, val roleService: RoleService) extends AbstractCRUDController[TimetableProtocol, Timetable] {

  override def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[Timetable]): Result = ???

  override implicit def reads: Reads[TimetableProtocol] = Timetable.reads

  override implicit def writes: Writes[Timetable] = Timetable.writes

  override implicit val mimeType: LwmMimeType = LwmMimeType.timetableV1Json

  override implicit def rdfReads: FromPG[Sesame, Timetable] = defaultBindings.TimetableBinding.timetableBinder

  override implicit def classUrisFor: ClassUrisFor[Sesame, Timetable] = defaultBindings.TimetableBinding.classUri

  override implicit def uriGenerator: UriGenerator[Timetable] = Timetable

  override implicit def rdfWrites: ToPG[Sesame, Timetable] = defaultBindings.TimetableBinding.timetableBinder

  override protected def fromInput(input: TimetableProtocol, id: Option[UUID]): Timetable = id match {
    case Some(uuid) => Timetable(input.labwork, input.entries, input.start, input.blacklist, input.buffer, uuid)
    case None => Timetable(input.labwork, input.entries, input.start, input.blacklist, input.buffer, Timetable.randomUUID)
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
    case _ => NonSecureBlock
  }

  def createFrom(labwork: String) = restrictedContext(labwork)(All) asyncAction { request =>
    super.all(NonSecureBlock)(request)
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
}