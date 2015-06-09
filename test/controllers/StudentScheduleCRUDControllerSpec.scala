package controllers

import models.schedules.StudentSchedule
import play.api.libs.json.Writes

class StudentScheduleCRUDControllerSpec extends AbstractCRUDControllerSpec[StudentSchedule] {
  override val entityToPass: StudentSchedule = StudentSchedule()

  override def entityTypeName: String = "StudentSchedule"

  override val controller: AbstractCRUDController[StudentSchedule] = new StudentScheduleCRUDController(repository, namespace)

  override val entityToFail: StudentSchedule = StudentSchedule()

  override implicit val jsonWrites: Writes[StudentSchedule] = StudentSchedule.writes

  override val mimeType: String = "application/json" //TODO: this should be a proper content type
}
