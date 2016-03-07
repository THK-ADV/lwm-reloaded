package controllers.security

import java.util.UUID

import base.TestBaseDefinition
import controllers.SessionController
import models.security.Permissions.authority
import models.security.RefRole
import org.mockito.Mockito._
import org.scalatest.WordSpec
import play.api.http.HeaderNames
import play.api.libs.json.Json
import play.api.test.{FakeHeaders, FakeRequest}
import play.api.test.Helpers._
import utils.LwmMimeType
import models.security.Permissions._

import scala.util.Success

class RefRoleControllerSecuritySpec extends WordSpec with TestBaseDefinition with SecurityBaseDefinition {

  "A RefRoleControllerSecuritySpec " should {

    "Allow non restricted context invocations when admin wants to get all refroles" in new FakeApplication() {
      import RefRole.writes

      when(roleService.authorityFor(FakeAdmin.toString)).thenReturn(Success(Some(FakeAdminAuth)))
      when(roleService.checkWith((None, refRole.getAll))(FakeAdminAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        GET,
        s"/refRoles"
      ).withSession(SessionController.userId -> FakeAdmin.toString)

      val result = route(request).get

      status(result) shouldBe OK
      contentAsJson(result) shouldBe Json.toJson(Set.empty[RefRole])
    }

    "Allow non restricted context invocations when rv wants to get a single refrole" in new FakeApplication() {
      import RefRole.writes

      when(roleService.authorityFor(FakeMv.toString)).thenReturn(Success(Some(FakeMvAuth)))
      when(roleService.checkWith((None, refRole.get))(FakeMvAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        GET,
        s"/refRoles/${UUID.randomUUID()}"
      ).withSession(SessionController.userId -> FakeMv.toString)

      val result = route(request).get

      status(result) shouldBe NOT_FOUND
    }
  }
}
