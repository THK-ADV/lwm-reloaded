package security

import java.util.UUID

import base.{SecurityBaseDefinition, TestBaseDefinition}
import controllers.SessionController
import models.SesameSemester$
import models.Permissions._
import org.joda.time.LocalDate
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

class SemesterControllerSecuritySpec extends WordSpec with TestBaseDefinition with SecurityBaseDefinition {

  "A SemesterControllerSecuritySpec " should {

    when(sessionService.isValid(Matchers.anyObject())).thenReturn(Future.successful(true))

    "Allow invocations when user is admin" in new FakeApplication() {
      when(roleService.authorities(FakeAdmin)).thenReturn(Success(Set(FakeAdminAuth)))
      when(roleService.checkAuthority((None, prime))(FakeAdminAuth)).thenReturn(Success(true))

      val json = Json.toJson(
        SesameSemester("label", "abbrev", LocalDate.now, LocalDate.now, LocalDate.now)
      )

      val request = FakeRequest(
        "POST",
        "/semesters",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> LwmMimeType.semesterV1Json)),
        json
      ).withSession(
        SessionController.userId -> FakeAdmin.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe CREATED
    }

    "Block invocations when user not an admin" in new FakeApplication() {
      when(roleService.authorities(FakeEmployee)).thenReturn(Success(Set(FakeEmployeeAuth)))
      when(roleService.checkAuthority((None, prime))(FakeEmployeeAuth)).thenReturn(Success(false))

      val json = Json.toJson(
        SesameSemester("label", "abbrev", LocalDate.now, LocalDate.now, LocalDate.now)
      )

      val request = FakeRequest(
        "POST",
        "/semesters",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> LwmMimeType.semesterV1Json)),
        json
      ).withSession(
        SessionController.userId -> FakeEmployee.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe UNAUTHORIZED
    }

    "Allow invocations when employee wants to get all semesters" in new FakeApplication() {
      when(roleService.authorities(FakeEmployee)).thenReturn(Success(Set(FakeEmployeeAuth)))
      when(roleService.checkAuthority((None, semester.getAll))(FakeEmployeeAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        "GET",
        "/semesters"
      ).withSession(
        SessionController.userId -> FakeEmployee.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe OK
    }

    "Block invocations when student wants to get all semesters" in new FakeApplication() {
      when(roleService.authorities(FakeStudent)).thenReturn(Success(Set(FakeStudentAuth)))
      when(roleService.checkAuthority((None, semester.getAll))(FakeStudentAuth)).thenReturn(Success(false))

      val request = FakeRequest(
        "GET",
        "/semesters"
      ).withSession(
        SessionController.userId -> FakeStudent.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe UNAUTHORIZED
    }
  }
}
