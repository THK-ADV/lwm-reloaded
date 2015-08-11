package controllers.crud

import java.util.UUID

import models.{Course, CourseProtocol}
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{JsValue, Json, Writes}
import play.api.mvc.Result
import utils.LWMMimeType

import scala.collection.Map

class CourseCRUDControllerSpec extends AbstractCRUDControllerSpec[CourseProtocol, Course] {
  override val entityToPass: Course = Course("label to pass", "lecturer to pass", Course.randomUUID)

  override def entityTypeName: String = "course"

  override val controller: AbstractCRUDController[CourseProtocol, Course] = new CourseCRUDController(repository, namespace, roleService) {
    override protected def fromInput(input: CourseProtocol, id: Option[UUID]) = entityToPass
  }

  override val entityToFail: Course = Course("label to fail", "lecturer to fail", Course.randomUUID)

  override implicit val jsonWrites: Writes[Course] = Course.writes

  override val mimeType: LWMMimeType = LWMMimeType.courseV1Json

  override val inputJson: JsValue = Json.obj(
    "label" -> "label input",
    "lecturer" -> "lecturer input"
  )

  import bindings.CourseBinding._
  import ops._

  override def pointedGraph: PointedGraph[Sesame] = entityToPass.toPG
}
