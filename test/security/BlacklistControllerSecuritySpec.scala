package security

import java.util.UUID

import base.{SecurityBaseDefinition, TestBaseDefinition}
import controllers.SessionController
import models.security.Permissions._
import models.semester.Blacklist
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

class BlacklistControllerSecuritySpec extends WordSpec with TestBaseDefinition with SecurityBaseDefinition {
  
  "A BlacklistControllerSecuritySpec " should {

    when(sessionService.isValid(Matchers.anyObject())).thenReturn(Future.successful(true))

    "Allow non restricted context invocations when admin wants to create a blacklist" in new FakeApplication() {
      import models.semester.Blacklist.writes
      
      when(roleService.authorities(FakeAdmin)).thenReturn(Success(Set(FakeAdminAuth)))
      when(roleService.checkAuthority((None, prime))(FakeAdminAuth)).thenReturn(Success(true))

      val json = Json.toJson(Blacklist.empty)

      val request = FakeRequest(
        POST,
        s"/blacklists",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> LwmMimeType.blacklistV1Json)),
        json
      ).withSession(
        SessionController.userId -> FakeAdmin.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe CREATED
    }

    "Allow non restricted context invocations when admin wants to get a single blacklist" in new FakeApplication() {
      when(roleService.authorities(FakeAdmin)).thenReturn(Success(Set(FakeAdminAuth)))
      when(roleService.checkAuthority((None, blacklist.get))(FakeAdminAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        GET,
        s"/blacklists/${UUID.randomUUID()}"
      ).withSession(
        SessionController.userId -> FakeAdmin.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe NOT_FOUND
    }

    "Allow non restricted context invocations when ma wants to get a single blacklist" in new FakeApplication() {
      when(roleService.authorities(FakeMa)).thenReturn(Success(Set(FakeMaAuth)))
      when(roleService.checkAuthority((None, blacklist.get))(FakeMaAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        GET,
        s"/blacklists/${UUID.randomUUID()}"
      ).withSession(
        SessionController.userId -> FakeMa.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe NOT_FOUND
    }

    "Block non restricted context invocations when ma wants to get all blacklists" in new FakeApplication() {
      when(roleService.authorities(FakeMa)).thenReturn(Success(Set(FakeMaAuth)))
      when(roleService.checkAuthority((None, prime))(FakeMaAuth)).thenReturn(Success(false))

      val request = FakeRequest(
        GET,
        s"/blacklists"
      ).withSession(
        SessionController.userId -> FakeMa.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe UNAUTHORIZED
    }
  }
}
