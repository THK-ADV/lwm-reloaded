package controllers.crud

import java.util.UUID

import models.Course
import models.security.{Permission, Role, RefRole, RefRoleProtocol}
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.http.HeaderNames
import play.api.libs.json.{Reads, Json, Writes, JsValue}
import play.api.test.{FakeHeaders, FakeRequest}
import play.api.test.Helpers._
import utils.LWMMimeType

import scala.util.Success

class RefRoleCRUDControllerSpec extends AbstractCRUDControllerSpec[RefRoleProtocol, RefRole] {
  import bindings.RefRoleBinding._
  import ops._

  override def entityTypeName: String = "refRole"

  override val controller: AbstractCRUDController[RefRoleProtocol, RefRole] = new RefRoleCRUDController(repository, namespace, roleService)

  override val entityToFail: RefRole = RefRole(Some(Course.randomUUID), Role("role to fail", Set(Permission("perm to pass"))), RefRole.randomUUID)

  override val entityToPass: RefRole = RefRole(Some(Course.randomUUID), Role("role to pass", Set(Permission("perm to fail"))), RefRole.randomUUID)

  override implicit val jsonWrites: Writes[RefRole] = RefRole.writes

  override val mimeType: LWMMimeType = LWMMimeType.refRoleV1Json

  override val pointedGraph: PointedGraph[Sesame] = entityToPass.toPG

  override val inputJson: JsValue = Json.obj(
    "module" -> Some(Course.randomUUID.toString),
    "role" -> Json.obj(
      "name" -> "role input",
      "permissions" -> Json.arr(Json.obj(
        "value" -> "perm"
      ))
    )
  )

  val noModuleJson: JsValue = Json.obj(
    "module" -> None,
    "role" -> Json.obj(
      "name" -> "no module",
      "permissions" -> Json.arr(Json.obj(
        "value" -> "no module"
      ))
    )
  )

  val noModuleRefRole = RefRole(None, Role("no module", Set(Permission("no module"))), RefRole.randomUUID)

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
