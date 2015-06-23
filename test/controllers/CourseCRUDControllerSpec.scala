package controllers

import controllers.crud.{AbstractCRUDController, CourseCRUDController}
import models.Course
import play.api.libs.json.Writes

class CourseCRUDControllerSpec extends AbstractCRUDControllerSpec[Course] {
  override val entityToPass: Course = Course("label to pass", "lecturer to pass")

  override def entityTypeName: String = "Course"

  override val controller: AbstractCRUDController[Course] = new CourseCRUDController(repository, namespace)

  override val entityToFail: Course = Course("label to fail", "lecturer to fail")

  override implicit val jsonWrites: Writes[Course] = Course.writes

  override val mimeType: String = "application/json" //TODO: this should be a proper content type
}
