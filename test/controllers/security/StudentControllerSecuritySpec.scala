package controllers.security

import java.util.UUID

import base.TestBaseDefinition
import controllers.SessionController
import models.security.Permissions._
import models.users.Student
import org.mockito.Mockito._
import org.scalatest.WordSpec
import play.api.libs.json.Json
import play.api.test.FakeRequest
import play.api.test.Helpers._

import scala.util.Success

class StudentControllerSecuritySpec extends WordSpec with TestBaseDefinition with SecurityBaseDefinition { 

  "A StudentControllerSecuritySpec " should {

    "Allow non restricted context invocations when user is an admin" in new FakeApplication() {
      when(roleService.authorityFor(FakeAdmin.toString)).thenReturn(Success(Some(FakeAdminAuth)))
      when(roleService.checkWith((None, user.get))(FakeAdminAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        GET,
        s"/students/${UUID.randomUUID()}"
      ).withSession(SessionController.userId -> FakeAdmin.toString)

      val result = route(request).get

      status(result) shouldBe NOT_FOUND
    }

    "Allow non restricted context invocations when employee wants to get a single student" in new FakeApplication() {
      when(roleService.authorityFor(FakeEmployee.toString)).thenReturn(Success(Some(FakeEmployeeAuth)))
      when(roleService.checkWith((None, user.get))(FakeEmployeeAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        GET,
        s"/students/${UUID.randomUUID()}"
      ).withSession(SessionController.userId -> FakeEmployee.toString)

      val result = route(request).get

      status(result) shouldBe NOT_FOUND
    }

    "Allow non restricted context invocations when employee wants to get all students" in new FakeApplication() {
      import Student.writes

      when(roleService.authorityFor(FakeEmployee.toString)).thenReturn(Success(Some(FakeEmployeeAuth)))
      when(roleService.checkWith((None, user.getAll))(FakeEmployeeAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        GET,
        s"/students"
      ).withSession(SessionController.userId -> FakeEmployee.toString)

      val result = route(request).get

      status(result) shouldBe OK
      contentAsJson(result) shouldBe Json.toJson(Set.empty[Student])
    }

    "Allow non restricted context invocations when student wants to get a single student" in new FakeApplication() {
      when(roleService.authorityFor(FakeStudent.toString)).thenReturn(Success(Some(FakeStudentAuth)))
      when(roleService.checkWith((None, user.get))(FakeStudentAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        GET,
        s"/students/${UUID.randomUUID()}"
      ).withSession(SessionController.userId -> FakeStudent.toString)

      val result = route(request).get

      status(result) shouldBe NOT_FOUND
    }

    "Block non restricted context invocations when student wants to get all students" in new FakeApplication() {
      when(roleService.authorityFor(FakeStudent.toString)).thenReturn(Success(Some(FakeStudentAuth)))
      when(roleService.checkWith((None, user.getAll))(FakeStudentAuth)).thenReturn(Success(false))

      val request = FakeRequest(
        GET,
        s"/students"
      ).withSession(SessionController.userId -> FakeStudent.toString)

      val result = route(request).get

      status(result) shouldBe UNAUTHORIZED
    }

    "Block non restricted context invocations when admin wants to delete a student" in new FakeApplication() {
      when(roleService.authorityFor(FakeAdmin.toString)).thenReturn(Success(Some(FakeAdminAuth)))
      when(roleService.checkWith((None, god))(FakeAdminAuth)).thenReturn(Success(false))

      val request = FakeRequest(
        DELETE,
        s"/students/${UUID.randomUUID()}"
      ).withSession(SessionController.userId -> FakeAdmin.toString)

      val result = route(request).get

      status(result) shouldBe UNAUTHORIZED
    }

    "Block non restricted context invocations when employee wants to delete a student" in new FakeApplication() {
      when(roleService.authorityFor(FakeEmployee.toString)).thenReturn(Success(Some(FakeEmployeeAuth)))
      when(roleService.checkWith((None, god))(FakeEmployeeAuth)).thenReturn(Success(false))

      val request = FakeRequest(
        DELETE,
        s"/students/${UUID.randomUUID()}"
      ).withSession(SessionController.userId -> FakeEmployee.toString)

      val result = route(request).get

      status(result) shouldBe UNAUTHORIZED
    }
  }
}
