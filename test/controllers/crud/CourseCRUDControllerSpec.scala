package controllers.crud

import java.util.UUID

import models.{Course, CourseProtocol}
import play.api.libs.json.{JsValue, Json, Writes}

class CourseCRUDControllerSpec extends AbstractCRUDControllerSpec[CourseProtocol, Course] {
  override val entityToPass: Course = Course("label to pass", "lecturer to pass", Course.randomUUID)

  override def entityTypeName: String = "Course"

  override val controller: AbstractCRUDController[CourseProtocol, Course] = new CourseCRUDController(repository, namespace) {
    override protected def fromInput(input: CourseProtocol, id: Option[UUID]) = entityToPass
  }

  override val entityToFail: Course = Course("label to fail", "lecturer to fail", Course.randomUUID)

  override implicit val jsonWrites: Writes[Course] = Course.writes

  override val mimeType: String = "application/json" //TODO: this should be a proper content type

  override val inputJson: JsValue = Json.obj(
    "label" -> "label input",
    "lecturer" -> "lecturer input"
  )
}
