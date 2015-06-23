package controllers

import java.util.UUID

import controllers.crud.{StudentScheduleAssociationCRUDController, AbstractCRUDController}
import models.schedules.{StudentScheduleAssociationProtocol, StudentScheduleAssociation}
import play.api.libs.json.{Json, JsValue, Writes}

class StudentScheduleAssociationCRUDControllerSpec extends AbstractCRUDControllerSpec[StudentScheduleAssociationProtocol, StudentScheduleAssociation] {
  override val entityToPass: StudentScheduleAssociation = StudentScheduleAssociation("date to pass", "groupScheduleAssociation to pass", "timetableEntry to pass", StudentScheduleAssociation.randomUUID)

  override def entityTypeName: String = "StudentScheduleAssociation"

  override val controller: AbstractCRUDController[StudentScheduleAssociationProtocol, StudentScheduleAssociation] = new StudentScheduleAssociationCRUDController(repository, namespace) {
    override protected def fromInput(input: StudentScheduleAssociationProtocol, id: Option[UUID]): StudentScheduleAssociation = entityToPass
  }

  override val entityToFail: StudentScheduleAssociation = StudentScheduleAssociation("date to fail", "groupScheduleAssociation to fail", "timetableEntry to fail", StudentScheduleAssociation.randomUUID)

  override implicit val jsonWrites: Writes[StudentScheduleAssociation] = StudentScheduleAssociation.writes

  override val mimeType: String = "application/json" //TODO: this should be a proper content type

  override val inputJson: JsValue = Json.obj(
    "date" -> "date input",
    "groupScheduleAssociation" -> "groupScheduleAssociation input",
    "timetableEntry" -> "timetableEntry"
  )
}