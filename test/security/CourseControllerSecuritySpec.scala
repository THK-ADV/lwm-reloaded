package security

import java.util.UUID

import base.{SecurityBaseDefinition, TestBaseDefinition}
import controllers.SessionController
import models.{SesameCourse, SesameRole, Roles}
import models.Permissions._
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

class CourseControllerSecuritySpec extends WordSpec with TestBaseDefinition with SecurityBaseDefinition {
  
  "A CourseControllerSecuritySpec " should {

    when(sessionService.isValid(Matchers.anyObject())).thenReturn(Future.successful(true))

    "Allow invocations when admin wants to create a course" in new FakeApplication() {
      val roles = Set(SesameRole(Roles.RightsManagerLabel, Set.empty), SesameRole(Roles.CourseManagerLabel, Set.empty))

      when(roleService.authorities(FakeAdmin)).thenReturn(Success(Set(FakeAdminAuth)))
      when(roleService.checkAuthority((None, prime))(FakeAdminAuth)).thenReturn(Success(true))
      when(roleService.rolesForCourse(Matchers.anyObject())).thenReturn(Success(roles))

      val json = Json.obj(
        "label" -> "",
        "description" -> "",
        "abbreviation" -> "",
        "lecturer" -> UUID.randomUUID,
        "semesterIndex" -> 1
      )

      val request = FakeRequest(
        POST,
        "/courses",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> LwmMimeType.courseV1Json)),
        json
      ).withSession(
        SessionController.userId -> FakeAdmin.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe CREATED
    }

    "Allow invocations when student wants to get a single course" in new FakeApplication() {
      when(roleService.authorities(FakeStudent)).thenReturn(Success(Set(FakeStudentAuth)))
      when(roleService.checkAuthority((None, course.get))(FakeStudentAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        GET,
        s"/courses/${UUID.randomUUID()}"
      ).withSession(
        SessionController.userId -> FakeStudent.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe NOT_FOUND
    }

    "Block invocations when student wants to get all courses" in new FakeApplication() {
      when(roleService.authorities(FakeStudent)).thenReturn(Success(Set(FakeStudentAuth)))
      when(roleService.checkAuthority((None, course.getAll))(FakeStudentAuth)).thenReturn(Success(false))

      val request = FakeRequest(
        GET,
        s"/courses"
      ).withSession(
        SessionController.userId -> FakeStudent.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe UNAUTHORIZED
    }

    "Allow invocations when employee wants to get all courses" in new FakeApplication() {
      when(roleService.authorities(FakeEmployee)).thenReturn(Success(Set(FakeEmployeeAuth)))
      when(roleService.checkAuthority((None, course.getAll))(FakeEmployeeAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        GET,
        s"/courses"
      ).withSession(
        SessionController.userId -> FakeEmployee.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe OK
    }

    "Allow invocations when a dedicated mv wants to update a course" in new FakeApplication() {
      when(roleService.authorities(FakeMv)).thenReturn(Success(Set(FakeMvAuth)))
      when(roleService.checkAuthority((Some(FakeCourse), course.update))(FakeMvAuth)).thenReturn(Success(true))

      val json = Json.toJson(SesameCourse("", "", "", UUID.randomUUID(), 1, None, FakeCourse))
      
      val request = FakeRequest(
        PUT,
        s"/courses/$FakeCourse",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> LwmMimeType.courseV1Json)),
        json
      ).withSession(
        SessionController.userId -> FakeMv.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe OK
    }

    "Allow invocations when admin wants to update a course" in new FakeApplication() {
      when(roleService.authorities(FakeAdmin)).thenReturn(Success(Set(FakeAdminAuth)))
      when(roleService.checkAuthority((Some(FakeCourse), course.update))(FakeAdminAuth)).thenReturn(Success(true))

      val json = Json.toJson(SesameCourse("", "", "", UUID.randomUUID(), 1, None, FakeCourse))
      
      val request = FakeRequest(
        PUT,
        s"/courses/$FakeCourse",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> LwmMimeType.courseV1Json)),
        json
      ).withSession(
        SessionController.userId -> FakeAdmin.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe OK
    }

    "Block invocations when employee wants to update a course" in new FakeApplication() {
      when(roleService.authorities(FakeEmployee)).thenReturn(Success(Set(FakeEmployeeAuth)))
      when(roleService.checkAuthority((Some(FakeCourse), course.update))(FakeEmployeeAuth)).thenReturn(Success(false))

      val json = Json.toJson(SesameCourse("", "", "", UUID.randomUUID(), 1, None, FakeCourse))

      val request = FakeRequest(
        PUT,
        s"/courses/$FakeCourse",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> LwmMimeType.courseV1Json)),
        json
      ).withSession(
        SessionController.userId -> FakeEmployee.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe UNAUTHORIZED
    }

    "Allow remaining invocations when user is an admin" in new FakeApplication() {
      when(roleService.authorities(FakeAdmin)).thenReturn(Success(Set(FakeAdminAuth)))
      when(roleService.checkAuthority((None, prime))(FakeAdminAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        DELETE,
        s"/courses/${UUID.randomUUID()}"
      ).withSession(
        SessionController.userId -> FakeAdmin.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe OK
    }
  }
}
