package security

import java.util.UUID

import base.{SecurityBaseDefinition, TestBaseDefinition}
import controllers.SessionController
import controllers.reportCard.ReportCardEntryTypeController
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
    import ReportCardEntryType._
    Json.toJson(ReportCardEntryType(Attendance.entryType, bool = true, 0, id))
  }

  "A ReportCardEntryTypeControllerSecuritySpec " should {

    "Allow restricted context invocations when admin wants to update a report card entry type" in new FakeApplication() {
      when(roleService.authorityFor(FakeAdmin.toString)).thenReturn(Success(Some(FakeAdminAuth)))
      when(roleService.checkWith((Some(FakeCourse), reportCardEntryType.update))(FakeAdminAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        PUT,
        s"$FakeCourseUri/reportCardEntries/${UUID.randomUUID}/types/$id",
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
      when(roleService.authorityFor(FakeMv.toString)).thenReturn(Success(Some(FakeMvAuth)))
      when(roleService.checkWith((Some(FakeCourse), reportCardEntryType.update))(FakeMvAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        PUT,
        s"$FakeCourseUri/reportCardEntries/${UUID.randomUUID}/types/$id",
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
      when(roleService.authorityFor(FakeMa.toString)).thenReturn(Success(Some(FakeMaAuth)))
      when(roleService.checkWith((Some(FakeCourse), reportCardEntryType.update))(FakeMaAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        PUT,
        s"$FakeCourseUri/reportCardEntries/${UUID.randomUUID}/types/$id",
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
      when(roleService.authorityFor(FakeStudent.toString)).thenReturn(Success(Some(FakeStudentAuth)))
      when(roleService.checkWith((Some(FakeCourse), reportCardEntryType.update))(FakeStudentAuth)).thenReturn(Success(false))

      val request = FakeRequest(
        PUT,
        s"$FakeCourseUri/reportCardEntries/${UUID.randomUUID}/types/$id",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> LwmMimeType.reportCardEntryTypeV1Json)),
        json
      ).withSession(
        SessionController.userId -> FakeStudent.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe UNAUTHORIZED
    }

    "Allow restricted context invocations when mv wants to get certain report card entry types by filters" in new FakeApplication() {
      when(roleService.authorityFor(FakeMa.toString)).thenReturn(Success(Some(FakeMaAuth)))
      when(roleService.checkWith((Some(FakeCourse), reportCardEntryType.getAll))(FakeMaAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        GET,
        s"$FakeCourseUri/reportCardEntries/types?${ReportCardEntryTypeController.studentAttribute}=${UUID.randomUUID}"
      ).withSession(
        SessionController.userId -> FakeMa.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe OK
    }

    "Bock restricted context invocations when student wants to get certain report card entry types by filters" in new FakeApplication() {
      when(roleService.authorityFor(FakeStudent.toString)).thenReturn(Success(Some(FakeStudentAuth)))
      when(roleService.checkWith((Some(FakeCourse), reportCardEntryType.getAll))(FakeStudentAuth)).thenReturn(Success(false))

      val request = FakeRequest(
        GET,
        s"$FakeCourseUri/reportCardEntries/types?${ReportCardEntryTypeController.studentAttribute}=${UUID.randomUUID}"
      ).withSession(
        SessionController.userId -> FakeStudent.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe UNAUTHORIZED
    }
  }
}
