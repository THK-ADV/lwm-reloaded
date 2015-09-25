package controllers.crud

import models.Course
import models.security.{Permission, RefRole, RefRoleProtocol, Role}
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.http.HeaderNames
import play.api.libs.json.{JsValue, Json, Writes}
import play.api.mvc.{Action, Result, AnyContent, Request}
import play.api.test.Helpers._
import play.api.test.{FakeHeaders, FakeRequest}
import utils.LWMActions.ContentTypedAction
import utils.LwmMimeType

import scala.util.Success

class RefRoleCRUDControllerSpec extends AbstractCRUDControllerSpec[RefRoleProtocol, RefRole] {

  import bindings.RefRoleBinding._
  import ops._

  override def entityTypeName: String = "refRole"

  override val controller: AbstractCRUDController[RefRoleProtocol, RefRole] = new RefRoleCRUDController(repository, namespace, roleService) {

    override protected def invokeAction(act: Rule)(moduleId: Option[String]): Block = new Block((None, Set())) {
      override def secured(block: (Request[AnyContent]) => Result): Action[AnyContent] = Action(block)

      override def securedt(block: (Request[JsValue]) => Result): Action[JsValue] = ContentTypedAction(block)(mimeType)
    }

  }

  override val entityToFail: RefRole = RefRole(Some(Course.randomUUID), Role.randomUUID, RefRole.randomUUID)

  override val entityToPass: RefRole = RefRole(Some(Course.randomUUID), Role.randomUUID, RefRole.randomUUID)

  override implicit val jsonWrites: Writes[RefRole] = RefRole.writes

  override val mimeType: LwmMimeType = LwmMimeType.refRoleV1Json

  override val pointedGraph: PointedGraph[Sesame] = entityToPass.toPG

  override val inputJson: JsValue = Json.obj(
    "module" -> Some(Course.randomUUID.toString),
    "role" -> Role.randomUUID
  )

  val noModuleJson: JsValue = Json.obj(
    "module" -> None,
    "role" -> Role.randomUUID.toString
  )

  val noModuleRefRole = RefRole(None, Role.randomUUID, RefRole.randomUUID)

  "A RefRoleCRUDControllerSpec also" should {
    s"handle refRoles with no module properly" in {
      when(repository.add(anyObject())(anyObject())).thenReturn(Success(noModuleRefRole.toPG))

      val request = FakeRequest(
        POST,
        s"/${entityTypeName}s",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> mimeType)),
        noModuleJson
      )
      val result = controller.create()(request)

      status(result) shouldBe CREATED
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(noModuleRefRole)
    }
  }
}
