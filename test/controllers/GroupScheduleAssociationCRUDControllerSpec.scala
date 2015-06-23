package controllers

import java.util.UUID

import controllers.crud.{AbstractCRUDController, GroupScheduleAssociationCRUDController}
import models.schedules.{GroupScheduleAssociationProtocol, GroupScheduleAssociation}
import play.api.libs.json.{Json, JsValue, Writes}

class GroupScheduleAssociationCRUDControllerSpec extends AbstractCRUDControllerSpec[GroupScheduleAssociationProtocol, GroupScheduleAssociation] {
  override val entityToPass: GroupScheduleAssociation = GroupScheduleAssociation("date to pass", "timetableEntry to pass", GroupScheduleAssociation.randomUUID)

  override def entityTypeName: String = "GroupScheduleAssociation"

  override val controller: AbstractCRUDController[GroupScheduleAssociationProtocol, GroupScheduleAssociation] = new GroupScheduleAssociationCRUDController(repository, namespace) {
    override protected def fromInput(input: GroupScheduleAssociationProtocol, id: Option[UUID]): GroupScheduleAssociation = entityToPass
  }

  override val entityToFail: GroupScheduleAssociation = GroupScheduleAssociation("date to fail", "timetableEntry to fail", GroupScheduleAssociation.randomUUID)

  override implicit val jsonWrites: Writes[GroupScheduleAssociation] = GroupScheduleAssociation.writes

  override val mimeType: String = "application/json" //TODO: this should be a proper content type

  override val inputJson: JsValue = Json.obj(
    "date" -> "date input",
    "timetableEntry" -> "timetableEntry input"
  )
}
