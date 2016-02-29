package security

import base.TestBaseDefinition
import controllers.crud.{ContentTyped, SecureControllerContext, Secured}
import models.security.Permission
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.scalatest.WordSpec
import org.scalatest.mock.MockitoSugar.mock
import play.api.mvc.Controller
import play.api.test.FakeRequest
import play.api.test.Helpers._
import services.RoleService
import utils.LwmMimeType

import scala.util.Success

class SecureControllerContextSpec extends WordSpec with TestBaseDefinition {
  self =>

  implicit val roleService = mock[RoleService]
  val controller = new MockController(roleService)

  val permission = Permission("permission")

  case class MockController(roleService: RoleService) extends Controller with SecureControllerContext with Secured with ContentTyped {
    override implicit val mimeType: LwmMimeType = LwmMimeType.authorityV1Json //not relevant

    def delegation = contextFrom(Create) asyncAction { request =>
      actualContent(NonSecureBlock)(request)
    }

    def actualContent(context: SecureContext = contextFrom(All)) = context action { request =>
      Ok("delegated")
    }

    override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
      case All => PartialSecureBlock(Set(permission))
      case _ => NonSecureBlock
    }
  }

  "A SecureControllerContext" should {

    "use default context block, if none other is provided" in {
      when(roleService.checkWith(anyObject())(anyObject())).thenReturn(Success(false))

      val fakeRequest = FakeRequest("GET", "/")

      val result = controller.actualContent()(fakeRequest)

      status(result) shouldBe UNAUTHORIZED
    }

    "always use the provided context block" in {

      val fakeRequest = FakeRequest("GET", "/")

      val result = controller.delegation(fakeRequest)

      status(result) shouldBe OK
      contentAsString(result) shouldBe "delegated"
    }

  }

}
