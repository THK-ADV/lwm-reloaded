package security

import base.TestBaseDefinition
import controllers.security.PermissionController
import models.security.Permissions
import org.mockito.Matchers
import org.mockito.Mockito.{mock => _, _}
import org.scalatest.WordSpec
import org.scalatest.mock.MockitoSugar._
import play.api.libs.json.Json
import play.api.test.FakeRequest
import play.api.test.Helpers._
import services.{RoleService, SessionHandlingService}
import store.{Namespace, SesameRepository}
import utils.LwmMimeType

import scala.concurrent.Future

class PermissionControllerSpec extends WordSpec with TestBaseDefinition {

  val repository = mock[SesameRepository]
  val roleService = mock[RoleService]
  val sessionService = mock[SessionHandlingService]
  val namespace = Namespace("http://testNamespace/")

  val controller = new PermissionController(repository, sessionService, namespace, roleService) {
    override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }
  }
  val mimeType = LwmMimeType.permissionV1Json

  "A PermissionControllerSpec " should {

    when(sessionService.isValid(Matchers.anyObject())).thenReturn(Future.successful(true))

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
