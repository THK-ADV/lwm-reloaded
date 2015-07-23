package controllers.crud

import java.util.UUID

import models.{Course, CourseProtocol}
import play.api.libs.json.{JsValue, Json, Writes}
import play.api.mvc.Result
import utils.LWMMimeType

import scala.collection.Map

class CourseCRUDControllerSpec extends AbstractCRUDControllerSpec[CourseProtocol, Course] {
  override val entityToPass: Course = Course("label to pass", "lecturer to pass", Course.randomUUID)

  override def entityTypeName: String = "course"

  override val controller: AbstractCRUDController[CourseProtocol, Course] = new CourseCRUDController(repository, namespace) {
    override protected def fromInput(input: CourseProtocol, id: Option[UUID]) = entityToPass
  }

  override val entityToFail: Course = Course("label to fail", "lecturer to fail", Course.randomUUID)

  override implicit val jsonWrites: Writes[Course] = Course.writes

  override val mimeType: LWMMimeType = LWMMimeType.courseV1Json

  override val inputJson: JsValue = Json.obj(
    "label" -> "label input",
    "lecturer" -> "lecturer input"
  )
}
