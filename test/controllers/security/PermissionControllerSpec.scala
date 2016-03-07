package controllers.security

import base.TestBaseDefinition
import models.security.Permissions
import org.scalatest.WordSpec
import org.scalatest.mock.MockitoSugar._
import play.api.libs.json.Json
import play.api.test.FakeRequest
import play.api.test.Helpers._
import services.RoleService
import store.{Namespace, SesameRepository}
import utils.LwmMimeType

class PermissionControllerSpec extends WordSpec with TestBaseDefinition {

  val repository = mock[SesameRepository]
  val roleService = mock[RoleService]
  val namespace = Namespace("http://testNamespace/")

  val controller = new PermissionController(repository, namespace, roleService) {
    override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }
  }
  val mimeType = LwmMimeType.permissionV1Json

  "A PermissionControllerSpec " should {
    "return all permissions" in {
      val permissions = Permissions.all

      val request = FakeRequest(
        GET,
        "permissions"
      )
      val result = controller.all()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(permissions)
    }

    "return the expected content type" in {
      val request = FakeRequest(
        HEAD,
        "permissions"
      )

      val result = controller.header()(request)

      status(result) shouldBe NO_CONTENT
      contentType(result) shouldBe Some[String](mimeType)
      contentAsString(result) shouldBe empty
    }
  }
}
