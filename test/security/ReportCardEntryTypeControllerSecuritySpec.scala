package security

import java.util.UUID

import base.{SecurityBaseDefinition, TestBaseDefinition}
import controllers.SessionController
import models.labwork.ReportCardEntryType
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

class ReportCardEntryTypeControllerSecuritySpec extends WordSpec with TestBaseDefinition with SecurityBaseDefinition {

  when(sessionService.isValid(Matchers.anyObject())).thenReturn(Future.successful(true))

  val id = UUID.randomUUID
  val json = {
    import models.labwork.ReportCardEntryType._
    import scala.util.Random.nextBoolean
    Json.toJson(ReportCardEntryType(Attendance.entryType, bool = nextBoolean, 0, None, id))
  }

  "A ReportCardEntryTypeControllerSecuritySpec " should {

    "Allow restricted context invocations when admin wants to update a report card entry type" in new FakeApplication() {
      when(roleService.authorities(FakeAdmin)).thenReturn(Success(Set(FakeAdminAuth)))
      when(roleService.checkAuthority((Some(FakeCourse), reportCardEntryType.update))(FakeAdminAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        PUT,
        s"$FakeCourseUri/reportCardEntryTypes/$id",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> LwmMimeType.reportCardEntryTypeV1Json)),
        json
      ).withSession(
        SessionController.userId -> FakeAdmin.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe OK
    }

    "Allow restricted context invocations when mv wants to update a report card entry type" in new FakeApplication() {
      when(roleService.authorities(FakeMv)).thenReturn(Success(Set(FakeMvAuth)))
      when(roleService.checkAuthority((Some(FakeCourse), reportCardEntryType.update))(FakeMvAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        PUT,
        s"$FakeCourseUri/reportCardEntryTypes/$id",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> LwmMimeType.reportCardEntryTypeV1Json)),
        json
      ).withSession(
        SessionController.userId -> FakeMv.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe OK
    }

    "Allow restricted context invocations when hk wants to update a report card entry type" in new FakeApplication() {
      when(roleService.authorities(FakeMa)).thenReturn(Success(Set(FakeMaAuth)))
      when(roleService.checkAuthority((Some(FakeCourse), reportCardEntryType.update))(FakeMaAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        PUT,
        s"$FakeCourseUri/reportCardEntryTypes/$id",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> LwmMimeType.reportCardEntryTypeV1Json)),
        json
      ).withSession(
        SessionController.userId -> FakeMa.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe OK
    }

    "Block restricted context invocations when student wants to update his own report card entry type" in new FakeApplication() {
      when(roleService.authorities(FakeStudent)).thenReturn(Success(Set(FakeStudentAuth)))
      when(roleService.checkAuthority((Some(FakeCourse), reportCardEntryType.update))(FakeStudentAuth)).thenReturn(Success(false))

      val request = FakeRequest(
        PUT,
        s"$FakeCourseUri/reportCardEntryTypes/$id",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> LwmMimeType.reportCardEntryTypeV1Json)),
        json
      ).withSession(
        SessionController.userId -> FakeStudent.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe UNAUTHORIZED
    }
  }
}
