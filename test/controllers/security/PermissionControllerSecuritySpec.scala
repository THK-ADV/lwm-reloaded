package controllers.security

import base.TestBaseDefinition
import controllers.SessionController
import models.security.Permissions
import org.mockito.Matchers
import org.mockito.Mockito._
import org.scalatest.WordSpec
import play.api.libs.json.Json
import play.api.test.FakeRequest
import play.api.test.Helpers._

import scala.concurrent.Future
import scala.util.Success

class PermissionControllerSecuritySpec extends WordSpec with TestBaseDefinition with SecurityBaseDefinition {

  "A PermissionControllerSecuritySpec " should {

    when(sessionService.isValid(Matchers.anyObject())).thenReturn(Future.successful(true))

    "Allow non restricted context invocations when admin wants to get all permissions" in new FakeApplication() {
      import models.security.Permission.writes

      when(roleService.authorityFor(FakeAdmin.toString)).thenReturn(Success(Some(FakeAdminAuth)))
      when(roleService.checkWith((None, Permissions.prime))(FakeAdminAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        GET,
        s"/permissions"
      ).withSession(SessionController.userId -> FakeAdmin.toString)

      val result = route(request).get

      status(result) shouldBe OK
      contentAsJson(result) shouldBe Json.toJson(Permissions.all)
    }

    "Block non restricted context invocations when mv wants to get all permissions" in new FakeApplication() {
      when(roleService.authorityFor(FakeMv.toString)).thenReturn(Success(Some(FakeMvAuth)))
      when(roleService.checkWith((None, Permissions.prime))(FakeMvAuth)).thenReturn(Success(false))

      val request = FakeRequest(
        GET,
        s"/permissions"
      ).withSession(SessionController.userId -> FakeMv.toString)

      val result = route(request).get

      status(result) shouldBe UNAUTHORIZED
    }
  }
}
