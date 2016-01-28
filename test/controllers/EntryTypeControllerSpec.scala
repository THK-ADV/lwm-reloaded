package controllers

import base.TestBaseDefinition
import models.{EntryType, EntryTypes}
import org.scalatest.WordSpec
import play.api.libs.json.{JsValue, Json}
import play.api.mvc.{Action, AnyContent, Request, Result}
import play.api.{Application, ApplicationLoader}
import play.api.ApplicationLoader.Context
import play.api.test.{FakeRequest, WithApplicationLoader}
import services.RoleService
import store.{Namespace, SesameRepository}
import utils.{LwmMimeType, DefaultLwmApplication}
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.scalatest.mock.MockitoSugar.mock
import EntryType._
import play.api.test.Helpers._
import utils.LWMActions.ContentTypedAction

class EntryTypeControllerSpec extends WordSpec with TestBaseDefinition {
  self =>

  val roleService = mock[RoleService]
  val repository = mock[SesameRepository]
  val ns = mock[Namespace]
  val mimeType = LwmMimeType.entryTypeV1Json

  val controller = new EntryTypeController(repository, ns, roleService) {
    override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }
  }

  "An EntryTypeController" should {

    "return all EntryTypes" in {
      val expectedEntryType = EntryTypes.types

      val request = FakeRequest(
        GET,
        "/entryTypes"
      )

      val result = controller.all()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(expectedEntryType)
    }

    "return the expected content type" in {

      val request = FakeRequest(
        HEAD,
        "/entryTypes"
      )

      val result = controller.header()(request)
      status(result) shouldBe NO_CONTENT
      contentType(result) shouldBe Some[String](mimeType)
      contentAsString(result) shouldBe empty
    }
  }

}
