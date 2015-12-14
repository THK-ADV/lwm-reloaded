package controllers.crud.schedule

import java.util.UUID

import controllers.crud.AbstractCRUDController
import models._
import models.schedule.{Timetable, Schedule, ScheduleProtocol, TimetableProtocol}
import models.security.Permissions._
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{JsError, Json, Reads, Writes}
import play.api.mvc.Result
import services.{TimetableServiceLike, RoleService}
import store.{Namespace, SesameRepository}
import utils.LwmMimeType

import scala.collection.Map

class ScheduleCRUDController(val repository: SesameRepository, val namespace: Namespace, val roleService: RoleService, val timetableService: TimetableServiceLike) extends AbstractCRUDController[ScheduleProtocol, Schedule] {

  override implicit def rdfWrites: ToPG[Sesame, Schedule] = defaultBindings.ScheduleBinding.scheduleBinder

  override implicit def rdfReads: FromPG[Sesame, Schedule] = defaultBindings.ScheduleBinding.scheduleBinder

  override implicit def classUrisFor: ClassUrisFor[Sesame, Schedule] = defaultBindings.ScheduleBinding.classUri

  override implicit def uriGenerator: UriGenerator[Schedule] = Schedule

  override protected def fromInput(input: ScheduleProtocol, id: Option[UUID]): Schedule = ???

  override def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[Schedule]): Result = ???

  override implicit def reads: Reads[ScheduleProtocol] = Schedule.reads

  override implicit def writes: Writes[Schedule] = Schedule.writes

  override implicit val mimeType: LwmMimeType = LwmMimeType.scheduleV1Json

  // /labworks/:labwork/schedules/timetable
  def fromTimetable(labwork: String) = restrictedContext(labwork)(Create) contentTypedAction { implicit request =>
    implicit val reads = Timetable.reads

    request.body.validate[TimetableProtocol].fold(
      errors => {
        BadRequest(Json.obj(
          "status" -> "KO",
          "errors" -> JsError.toJson(errors)
        ))
      },
      success => {
        ???
      })
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case _ => PartialSecureBlock(Set(prime))
  }

  override protected def restrictedContext(moduleId: String): PartialFunction[Rule, SecureContext] = {
    case Create => SecureBlock(moduleId, Set(createSchedule))
    case _ => NonSecureBlock
  }
}