package security

import java.util.UUID

import base.StreamHandler._
import base.{SecurityBaseDefinition, TestBaseDefinition}
import controllers.SessionController
import models.security.Permissions._
import org.mockito.Matchers
import org.mockito.Mockito._
import org.scalatest.WordSpec
import play.api.http.HeaderNames
import play.api.test.Helpers._
import play.api.test.{FakeHeaders, FakeRequest}
import utils.LwmMimeType

import scala.concurrent.Future
import scala.util.Success

class GroupControllerSecuritySpec extends WordSpec with TestBaseDefinition with SecurityBaseDefinition { 
  
  "A GroupControllerSecuritySpec " should {

    when(sessionService.isValid(Matchers.anyObject())).thenReturn(Future.successful(true))

    "Allow restricted invocations when admin wants to get all groups" in new FakeApplication() {
      when(roleService.authorityFor(FakeAdmin)).thenReturn(Success(Set(FakeAdminAuth)))
      when(roleService.checkWith((Some(FakeCourse), group.getAll))(FakeAdminAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        GET,
        s"$FakeCourseUri/labworks/${UUID.randomUUID()}/groups"
      ).withSession(
        SessionController.userId -> FakeAdmin.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe OK
      contentFromStream(result) shouldBe emptyJson
    }

    "Allow restricted invocations when mv wants to create a group" in new FakeApplication() {
      when(roleService.authorityFor(FakeMv)).thenReturn(Success(Set(FakeMvAuth)))
      when(roleService.checkWith((Some(FakeCourse), group.create))(FakeMvAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        POST,
        s"$FakeCourseUri/labworks/${UUID.randomUUID()}/groups/count?value=10"
      ).withSession(
        SessionController.userId -> FakeMv.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe INTERNAL_SERVER_ERROR
    }

    "Allow restricted invocations when mv wants to get a single group" in new FakeApplication() {
      when(roleService.authorityFor(FakeMv)).thenReturn(Success(Set(FakeMvAuth)))
      when(roleService.checkWith((Some(FakeCourse), group.get))(FakeMvAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        GET,
        s"$FakeCourseUri/labworks/${UUID.randomUUID()}/groups/${UUID.randomUUID()}"
      ).withSession(
        SessionController.userId -> FakeMv.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe NOT_FOUND
    }

    "Allow restricted invocations when ma wants to get a single group" in new FakeApplication() {
      when(roleService.authorityFor(FakeMa)).thenReturn(Success(Set(FakeMaAuth)))
      when(roleService.checkWith((Some(FakeCourse), group.get))(FakeMaAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        GET,
        s"$FakeCourseUri/labworks/${UUID.randomUUID()}/groups/${UUID.randomUUID()}"
      ).withSession(
        SessionController.userId -> FakeMa.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe NOT_FOUND
    }

    "Block restricted invocations when ma wants to get all group" in new FakeApplication() {
      when(roleService.authorityFor(FakeMa)).thenReturn(Success(Set(FakeMaAuth)))
      when(roleService.checkWith((Some(FakeCourse), group.getAll))(FakeMaAuth)).thenReturn(Success(false))

      val request = FakeRequest(
        GET,
        s"$FakeCourseUri/labworks/${UUID.randomUUID()}/groups"
      ).withSession(
        SessionController.userId -> FakeMa.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe UNAUTHORIZED
    }
  }
}
