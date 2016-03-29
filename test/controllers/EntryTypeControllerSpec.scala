package controllers

import base.TestBaseDefinition
import models.labwork.AssignmentEntryType
import models.labwork.AssignmentEntryType._
import org.scalatest.WordSpec
import org.scalatest.mock.MockitoSugar.mock
import play.api.libs.json.Json
import play.api.test.FakeRequest
import play.api.test.Helpers._
import services.{RoleService, SessionHandlingService}
import store.{Namespace, SesameRepository}
import utils.LwmMimeType

class EntryTypeControllerSpec extends WordSpec with TestBaseDefinition {
  self =>

  val roleService = mock[RoleService]
  val sessionService = mock[SessionHandlingService]
  val repository = mock[SesameRepository]
  val ns = mock[Namespace]
  val mimeType = LwmMimeType.entryTypeV1Json

  val controller = new EntryTypeController(repository, sessionService, ns, roleService) {
    override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }
  }

  "An EntryTypeController" should {

    "return all EntryTypes" in {
      val expectedEntryType = AssignmentEntryType.all

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
