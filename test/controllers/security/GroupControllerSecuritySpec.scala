package controllers.security

import java.util.UUID

import base.TestBaseDefinition
import controllers.SessionController
import models.Group
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

class GroupControllerSecuritySpec extends WordSpec with TestBaseDefinition with SecurityBaseDefinition { 
  
  "A GroupControllerSecuritySpec " should {

    when(sessionService.isValid(Matchers.anyObject())).thenReturn(Future.successful(true))

    "Allow non restricted context invocations when user is an admin" in new FakeApplication() {
      when(roleService.authorityFor(FakeAdmin.toString)).thenReturn(Success(Some(FakeAdminAuth)))
      when(roleService.checkWith((None, group.get))(FakeAdminAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        GET,
        s"/groups/${UUID.randomUUID()}"
      ).withSession(SessionController.userId -> FakeAdmin.toString)

      val result = route(request).get

      status(result) shouldBe NOT_FOUND
    }

    "Allow restricted invocations when admin wants to get all groups" in new FakeApplication() {
      import Group.writes

      when(roleService.authorityFor(FakeAdmin.toString)).thenReturn(Success(Some(FakeAdminAuth)))
      when(roleService.checkWith((Some(FakeCourse), group.getAll))(FakeAdminAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        GET,
        s"$FakeCourseUri/groups"
      ).withSession(SessionController.userId -> FakeAdmin.toString)

      val result = route(request).get

      status(result) shouldBe OK
      contentAsJson(result) shouldBe Json.toJson(Set.empty[Group])
    }

    "Allow restricted invocations when mv wants to create a group" in new FakeApplication() {
      when(roleService.authorityFor(FakeMv.toString)).thenReturn(Success(Some(FakeMvAuth)))
      when(roleService.checkWith((Some(FakeCourse), group.create))(FakeMvAuth)).thenReturn(Success(true))

      val json = Json.obj(
        "labwork" -> UUID.randomUUID(),
        "count" -> 10
      )

      val request = FakeRequest(
        POST,
        s"$FakeCourseUri/groups/count",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> LwmMimeType.groupV1Json)),
        json
      ).withSession(SessionController.userId -> FakeMv.toString)

      val result = route(request).get

      status(result) shouldBe INTERNAL_SERVER_ERROR
    }

    "Allow restricted invocations when mv wants to get a single group" in new FakeApplication() {
      when(roleService.authorityFor(FakeMv.toString)).thenReturn(Success(Some(FakeMvAuth)))
      when(roleService.checkWith((Some(FakeCourse), group.get))(FakeMvAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        GET,
        s"$FakeCourseUri/groups/${UUID.randomUUID()}"
      ).withSession(SessionController.userId -> FakeMv.toString)

      val result = route(request).get

      status(result) shouldBe NOT_FOUND
    }

    "Allow restricted invocations when ma wants to get a single group" in new FakeApplication() {
      when(roleService.authorityFor(FakeMa.toString)).thenReturn(Success(Some(FakeMaAuth)))
      when(roleService.checkWith((Some(FakeCourse), group.get))(FakeMaAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        GET,
        s"$FakeCourseUri/groups/${UUID.randomUUID()}"
      ).withSession(SessionController.userId -> FakeMa.toString)

      val result = route(request).get

      status(result) shouldBe NOT_FOUND
    }

    "Block restricted invocations when ma wants to get all group" in new FakeApplication() {
      when(roleService.authorityFor(FakeMa.toString)).thenReturn(Success(Some(FakeMaAuth)))
      when(roleService.checkWith((Some(FakeCourse), group.getAll))(FakeMaAuth)).thenReturn(Success(false))

      val request = FakeRequest(
        GET,
        s"$FakeCourseUri/groups"
      ).withSession(SessionController.userId -> FakeMa.toString)

      val result = route(request).get

      status(result) shouldBe UNAUTHORIZED
    }

    "Allow non restricted invocations when student wants to get a single group" in new FakeApplication() {
      when(roleService.authorityFor(FakeStudent.toString)).thenReturn(Success(Some(FakeStudentAuth)))
      when(roleService.checkWith((None, group.get))(FakeStudentAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        GET,
        s"/groups/${UUID.randomUUID()}"
      ).withSession(SessionController.userId -> FakeStudent.toString)

      val result = route(request).get

      status(result) shouldBe NOT_FOUND
    }

    "Block non restricted invocations when student wants to get all groups because route is not found" in new FakeApplication() {
      when(roleService.authorityFor(FakeStudent.toString)).thenReturn(Success(Some(FakeStudentAuth)))
      when(roleService.checkWith((None, group.getAll))(FakeStudentAuth)).thenReturn(Success(false))

      val request = FakeRequest(
        GET,
        s"/groups"
      ).withSession(SessionController.userId -> FakeStudent.toString)

      val result = route(request).get

      status(result) shouldBe NOT_FOUND
    }

    "Block non restricted invocations when employee wants to get a single group" in new FakeApplication {
      when(roleService.authorityFor(FakeEmployee.toString)).thenReturn(Success(Some(FakeEmployeeAuth)))
      when(roleService.checkWith((None, group.get))(FakeEmployeeAuth)).thenReturn(Success(false))

      val request = FakeRequest(
        GET,
        s"/groups/${UUID.randomUUID()}"
      ).withSession(SessionController.userId -> FakeEmployee.toString)

      val result = route(request).get

      status(result) shouldBe UNAUTHORIZED
    }
  }
}
