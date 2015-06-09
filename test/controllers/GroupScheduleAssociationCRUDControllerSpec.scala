package controllers

import models.schedules.GroupScheduleAssociation
import play.api.libs.json.Writes

class GroupScheduleAssociationCRUDControllerSpec extends AbstractCRUDControllerSpec[GroupScheduleAssociation] {
  override val entityToPass: GroupScheduleAssociation = GroupScheduleAssociation("date to pass", "timetableEntry to pass")

  override def entityTypeName: String = "GroupScheduleAssociation"

  override val controller: AbstractCRUDController[GroupScheduleAssociation] = new GroupScheduleAssociationCRUDController(repository, namespace)

  override val entityToFail: GroupScheduleAssociation = GroupScheduleAssociation("date to fail", "timetableEntry to fail")

  override implicit val jsonWrites: Writes[GroupScheduleAssociation] = GroupScheduleAssociation.writes

  override val mimeType: String = "application/json" //TODO: this should be a proper content type
}
