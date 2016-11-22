package controllers

import base.TestBaseDefinition
import models.AssignmentEntryType
import org.scalatest.WordSpec
import org.scalatest.mock.MockitoSugar.mock
import play.api.libs.json.{Json, Writes}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import services.{RoleService, SessionHandlingService}
import store.{Namespace, SesameRepository}
import utils.LwmMimeType

class EntryTypeControllerSpec extends WordSpec with TestBaseDefinition { self =>

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
      contentType(result) shouldBe Some(mimeType.value)
      contentAsJson(result) shouldBe Json.toJson(expectedEntryType)(Writes.set(AssignmentEntryType.writes))
    }

    "return the expected content type" in {

      val request = FakeRequest(
        HEAD,
        "/entryTypes"
      )

      val result = controller.header()(request)
      status(result) shouldBe NO_CONTENT
      contentType(result) shouldBe Some(mimeType.value)
      contentAsString(result) shouldBe empty
    }
  }

}
