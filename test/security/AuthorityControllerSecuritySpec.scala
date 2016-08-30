package security

import java.util.UUID

import base.{SecurityBaseDefinition, TestBaseDefinition}
import controllers.SessionController
import models.security.Permissions._
import org.mockito.Matchers
import org.mockito.Mockito._
import org.scalatest.WordSpec
import play.api.http.HeaderNames
import play.api.libs.json.Json
import play.api.test.Helpers._
import play.api.test.{FakeHeaders, FakeRequest}
import utils.LwmMimeType

import scala.concurrent.Future
import scala.util.Success

class AuthorityControllerSecuritySpec extends WordSpec with TestBaseDefinition with SecurityBaseDefinition {

  "A AuthorityControllerSecuritySpec " should {

    when(sessionService.isValid(Matchers.anyObject())).thenReturn(Future.successful(true))

    "Allow non restricted context invocations when admin wants to create an authority" in new FakeApplication() {
      when(roleService.authorities(FakeAdmin)).thenReturn(Success(Set(FakeAdminAuth)))
      when(roleService.checkAuthority((None, authority.create))(FakeAdminAuth)).thenReturn(Success(true))

      val json = Json.obj(
        "user" -> UUID.randomUUID(),
        "role" -> UUID.randomUUID(),
        "course" -> UUID.randomUUID()
      )

      val request = FakeRequest(
        POST,
        s"/authorities",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> LwmMimeType.authorityV1Json)),
        json
      ).withSession(
        SessionController.userId -> FakeAdmin.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe CREATED
    }

    "Allow non restricted context invocations when rv wants to create an authority" in new FakeApplication() {
      when(roleService.authorities(FakeRv)).thenReturn(Success(Set(FakeRvAuth)))
      when(roleService.checkAuthority((None, authority.create))(FakeRvAuth)).thenReturn(Success(true))

      val json = Json.obj(
        "user" -> UUID.randomUUID(),
        "role" -> UUID.randomUUID(),
        "course" -> UUID.randomUUID()
      )

      val request = FakeRequest(
        POST,
        s"/authorities",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> LwmMimeType.authorityV1Json)),
        json
      ).withSession(
        SessionController.userId -> FakeRv.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe CREATED
    }

    "Allow non restricted context invocations when rv wants to get a single authority" in new FakeApplication() {
      when(roleService.authorities(FakeRv)).thenReturn(Success(Set(FakeRvAuth)))
      when(roleService.checkAuthority((None, authority.get))(FakeRvAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        GET,
        s"/authorities/${UUID.randomUUID()}"
      ).withSession(
        SessionController.userId -> FakeRv.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe NOT_FOUND
    }
  }
}
