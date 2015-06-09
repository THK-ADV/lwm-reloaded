package controllers

import models.Semester
import play.api.libs.json.Writes

class SemesterCRUDControllerSpec extends AbstractCRUDControllerSpec[Semester] {
  override val entityToPass: Semester = Semester("name to pass", "startDate to pass", "endDate to pass")

  override def entityTypeName: String = "Semester"

  override val controller: AbstractCRUDController[Semester] = new SemesterCRUDController(repository, namespace)

  override val entityToFail: Semester = Semester("name to fail", "startDate to fail", "endDate to fail")

  override implicit val jsonWrites: Writes[Semester] = Semester.writes

  override val mimeType: String = "application/json" //TODO: this should be a proper content type
}
