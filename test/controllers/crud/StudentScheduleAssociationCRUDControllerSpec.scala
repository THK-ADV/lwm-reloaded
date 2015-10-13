package controllers.crud

import java.util.UUID

import models.schedules.{StudentScheduleAssociation, StudentScheduleAssociationProtocol}
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{JsValue, Json, Writes}
import play.api.mvc.{Action, Result, AnyContent, Request}
import utils.LWMActions.ContentTypedAction
import utils.LwmMimeType

class StudentScheduleAssociationCRUDControllerSpec extends AbstractCRUDControllerSpec[StudentScheduleAssociationProtocol, StudentScheduleAssociation] {
  override val entityToPass: StudentScheduleAssociation = StudentScheduleAssociation("date to pass", "groupScheduleAssociation to pass", "timetableEntry to pass", StudentScheduleAssociation.randomUUID)

  override def entityTypeName: String = "studentScheduleAssociation"

  override val controller: AbstractCRUDController[StudentScheduleAssociationProtocol, StudentScheduleAssociation] = new StudentScheduleAssociationCRUDController(repository, namespace, roleService) {

    override protected def invokeAction(act: Rule)(moduleId: Option[String]): Block = new Block((None, Set())) {
      override def secured(block: (Request[AnyContent]) => Result): Action[AnyContent] = Action(block)
      override def secureContentTyped(block: (Request[JsValue]) => Result): Action[JsValue] = ContentTypedAction(block)(mimeType)
    }

    override protected def fromInput(input: StudentScheduleAssociationProtocol, id: Option[UUID]): StudentScheduleAssociation = entityToPass
  }

  override val entityToFail: StudentScheduleAssociation = StudentScheduleAssociation("date to fail", "groupScheduleAssociation to fail", "timetableEntry to fail", StudentScheduleAssociation.randomUUID)

  override implicit val jsonWrites: Writes[StudentScheduleAssociation] = StudentScheduleAssociation.writes

  override val mimeType: LwmMimeType = LwmMimeType.studentScheduleAssociationV1Json

  override val inputJson: JsValue = Json.obj(
    "date" -> "date input",
    "groupScheduleAssociation" -> "groupScheduleAssociation input",
    "timetableEntry" -> "timetableEntry"
  )

  import bindings.StudentScheduleAssociationBinding._
  import ops._

  override def pointedGraph: PointedGraph[Sesame] = entityToPass.toPG
}