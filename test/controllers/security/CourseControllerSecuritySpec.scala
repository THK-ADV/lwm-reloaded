package controllers.security

import java.util.UUID

import base.TestBaseDefinition
import controllers.SessionController
import models.Course
import models.security.Permissions._
import org.mockito.Mockito._
import org.scalatest.WordSpec
import play.api.http.HeaderNames
import play.api.libs.json.Json
import play.api.test.Helpers._
import play.api.test.{FakeHeaders, FakeRequest}
import utils.LwmMimeType

import scala.util.Success

class CourseControllerSecuritySpec extends WordSpec with TestBaseDefinition with SecurityBaseDefinition {
  
  "A CourseControllerSecuritySpec " should {

    "Allow invocations when student wants to get a single course" in new FakeApplication() {
      when(roleService.authorityFor(FakeStudent.toString)).thenReturn(Success(Some(FakeStudentAuth)))
      when(roleService.checkWith((None, course.get))(FakeStudentAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        GET,
        s"/courses/${UUID.randomUUID()}"
      ).withSession(SessionController.userId -> FakeStudent.toString)

      val result = route(request).get

      status(result) shouldBe NOT_FOUND
    }

    "Block invocations when student wants to get all courses" in new FakeApplication() {
      when(roleService.authorityFor(FakeStudent.toString)).thenReturn(Success(Some(FakeStudentAuth)))
      when(roleService.checkWith((None, course.getAll))(FakeStudentAuth)).thenReturn(Success(false))

      val request = FakeRequest(
        GET,
        s"/courses"
      ).withSession(SessionController.userId -> FakeStudent.toString)

      val result = route(request).get

      status(result) shouldBe UNAUTHORIZED
    }

    "Allow invocations when employee wants to get all courses" in new FakeApplication() {
      when(roleService.authorityFor(FakeEmployee.toString)).thenReturn(Success(Some(FakeEmployeeAuth)))
      when(roleService.checkWith((None, course.getAll))(FakeEmployeeAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        GET,
        s"/courses"
      ).withSession(SessionController.userId -> FakeEmployee.toString)

      val result = route(request).get

      status(result) shouldBe OK
    }

    "Allow invocations when a dedicated mv wants to update a course" in new FakeApplication() {
      when(roleService.authorityFor(FakeMv.toString)).thenReturn(Success(Some(FakeMvAuth)))
      when(roleService.checkWith((Some(FakeCourse), course.update))(FakeMvAuth)).thenReturn(Success(true))

      val json = Json.toJson(Course("", "", "", UUID.randomUUID(), 1, FakeCourse))
      
      val request = FakeRequest(
        PUT,
        s"/courses/$FakeCourse",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> LwmMimeType.courseV1Json)),
        json
      ).withSession(SessionController.userId -> FakeMv.toString)

      val result = route(request).get

      status(result) shouldBe CREATED
    }

    "Allow invocations when admin wants to update a course" in new FakeApplication() {
      when(roleService.authorityFor(FakeAdmin.toString)).thenReturn(Success(Some(FakeAdminAuth)))
      when(roleService.checkWith((Some(FakeCourse), course.update))(FakeAdminAuth)).thenReturn(Success(true))

      val json = Json.toJson(Course("", "", "", UUID.randomUUID(), 1, FakeCourse))
      
      val request = FakeRequest(
        PUT,
        s"/courses/$FakeCourse",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> LwmMimeType.courseV1Json)),
        json
      ).withSession(SessionController.userId -> FakeAdmin.toString)

      val result = route(request).get

      status(result) shouldBe CREATED
    }

    "Block invocations when employee wants to update a course" in new FakeApplication() {
      when(roleService.authorityFor(FakeEmployee.toString)).thenReturn(Success(Some(FakeEmployeeAuth)))
      when(roleService.checkWith((Some(FakeCourse), course.update))(FakeEmployeeAuth)).thenReturn(Success(false))

      val json = Json.toJson(Course("", "", "", UUID.randomUUID(), 1, FakeCourse))

      val request = FakeRequest(
        PUT,
        s"/courses/$FakeCourse",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> LwmMimeType.courseV1Json)),
        json
      ).withSession(SessionController.userId -> FakeEmployee.toString)

      val result = route(request).get

      status(result) shouldBe UNAUTHORIZED
    }

    "Block invocations when either student, employee or mv wants to create a course" in new FakeApplication() {
      when(roleService.authorityFor(FakeEmployee.toString)).thenReturn(Success(Some(FakeEmployeeAuth)))
      when(roleService.checkWith((None, prime))(FakeEmployeeAuth)).thenReturn(Success(false))

      val json = Json.obj(
        "label" -> "",
        "description" -> "",
        "abbreviation" -> "",
        "lecturer" -> "",
        "semesterIndex" -> 1
      )

      val request = FakeRequest(
        POST,
        "/courses",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> LwmMimeType.courseV1Json)),
        json
      ).withSession(SessionController.userId -> FakeEmployee.toString)

      val result = route(request).get

      status(result) shouldBe UNAUTHORIZED
    }

    "Allow remaining invocations when user is an admin" in new FakeApplication() {
      when(roleService.authorityFor(FakeAdmin.toString)).thenReturn(Success(Some(FakeAdminAuth)))
      when(roleService.checkWith((None, prime))(FakeAdminAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        DELETE,
        s"/courses/${UUID.randomUUID()}"
      ).withSession(SessionController.userId -> FakeAdmin.toString)

      val result = route(request).get

      status(result) shouldBe OK
    }
  }
}
