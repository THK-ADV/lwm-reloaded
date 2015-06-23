package controllers

import controllers.crud.{StudentScheduleAssociationCRUDController, AbstractCRUDController}
import models.schedules.StudentScheduleAssociation
import play.api.libs.json.Writes

class StudentScheduleAssociationCRUDControllerSpec extends AbstractCRUDControllerSpec[StudentScheduleAssociation] {
  override val entityToPass: StudentScheduleAssociation = StudentScheduleAssociation("date to pass", "groupScheduleAssociation to pass", "timetableEntry to pass")

  override def entityTypeName: String = "StudentScheduleAssociation"

  override val controller: AbstractCRUDController[StudentScheduleAssociation] = new StudentScheduleAssociationCRUDController(repository, namespace)

  override val entityToFail: StudentScheduleAssociation = StudentScheduleAssociation("date to fail", "groupScheduleAssociation to fail", "timetableEntry to fail")

  override implicit val jsonWrites: Writes[StudentScheduleAssociation] = StudentScheduleAssociation.writes

  override val mimeType: String = "application/json" //TODO: this should be a proper content type
}