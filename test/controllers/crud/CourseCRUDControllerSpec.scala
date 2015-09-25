package controllers.crud

import java.util.UUID

import models.{Course, CourseProtocol}
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{JsValue, Json, Writes}
import play.api.mvc.{Action, Result, AnyContent, Request}
import utils.LWMActions.ContentTypedAction
import utils.LwmMimeType

class CourseCRUDControllerSpec extends AbstractCRUDControllerSpec[CourseProtocol, Course] {
  override val entityToPass: Course = Course("label to pass", "lecturer to pass", Course.randomUUID)

  override def entityTypeName: String = "course"

  override val controller: AbstractCRUDController[CourseProtocol, Course] = new CourseCRUDController(repository, namespace, roleService) {

    override protected def invokeAction(act: Rule)(moduleId: Option[String]): Block = new Block((None, Set())) {
      override def secured(block: (Request[AnyContent]) => Result): Action[AnyContent] = Action(block)
      override def securedt(block: (Request[JsValue]) => Result): Action[JsValue] = ContentTypedAction(block)(mimeType)
    }

    override protected def fromInput(input: CourseProtocol, id: Option[UUID]) = entityToPass
  }

  override val entityToFail: Course = Course("label to fail", "lecturer to fail", Course.randomUUID)

  override implicit val jsonWrites: Writes[Course] = Course.writes

  override val mimeType: LwmMimeType = LwmMimeType.courseV1Json

  override val inputJson: JsValue = Json.obj(
    "label" -> "label input",
    "lecturer" -> "lecturer input"
  )

  import bindings.CourseBinding._
  import ops._

  override val pointedGraph: PointedGraph[Sesame] = entityToPass.toPG
}
