package controllers.security

import java.util.UUID

import base.TestBaseDefinition
import controllers.SessionController
import models.labwork.{Rescheduled, ReportCardEntryType, ReportCardEntry}
import models.security.Permissions.reportCardEntry
import org.joda.time.{LocalDate, LocalTime}
import org.mockito.Matchers
import org.mockito.Mockito._
import org.scalatest.WordSpec
import play.api.http.HeaderNames
import play.api.libs.json.Json
import play.api.test.{FakeHeaders, FakeRequest}
import play.api.test.Helpers._
import utils.LwmMimeType

import scala.concurrent.Future
import scala.util.Success

class ReportCardEntryControllerSecuritySpec extends WordSpec with TestBaseDefinition with SecurityBaseDefinition {

  when(sessionService.isValid(Matchers.anyObject())).thenReturn(Future.successful(true))

  val json = {
    import ReportCardEntry.writes

    val rescheduled = Rescheduled(LocalDate.now.plusDays(1), LocalTime.now.plusHours(1), LocalTime.now.plusHours(1), UUID.randomUUID)
    val entry = ReportCardEntry(0, "", LocalDate.now, LocalTime.now, LocalTime.now, UUID.randomUUID, Set.empty[ReportCardEntryType], Some(rescheduled))
    Json.toJson(entry)
  }

  "A ReportCardEntryControllerSecuritySpec " should {

    "Allow non restricted context invocations when admin wants to reschedule a report card entry" in new FakeApplication() {
      when(roleService.authorityFor(FakeAdmin.toString)).thenReturn(Success(Some(FakeAdminAuth)))
      when(roleService.checkWith((Some(FakeCourse), reportCardEntry.update))(FakeAdminAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        PUT,
        s"$FakeCourseUri/reportCards/${UUID.randomUUID}/entries/${UUID.randomUUID}",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> LwmMimeType.reportCardEntryV1Json)),
        json
      ).withSession(
        SessionController.userId -> FakeAdmin.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe OK
    }

    "Allow non restricted context invocations when mv wants to reschedule a report card entry" in new FakeApplication() {
      when(roleService.authorityFor(FakeMv.toString)).thenReturn(Success(Some(FakeMvAuth)))
      when(roleService.checkWith((Some(FakeCourse), reportCardEntry.update))(FakeMvAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        PUT,
        s"$FakeCourseUri/reportCards/${UUID.randomUUID}/entries/${UUID.randomUUID}",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> LwmMimeType.reportCardEntryV1Json)),
        json
      ).withSession(
        SessionController.userId -> FakeMv.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe OK
    }

    "Allow non restricted context invocations when ma wants to reschedule a report card entry" in new FakeApplication() {
      when(roleService.authorityFor(FakeMa.toString)).thenReturn(Success(Some(FakeMaAuth)))
      when(roleService.checkWith((Some(FakeCourse), reportCardEntry.update))(FakeMaAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        PUT,
        s"$FakeCourseUri/reportCards/${UUID.randomUUID}/entries/${UUID.randomUUID}",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> LwmMimeType.reportCardEntryV1Json)),
        json
      ).withSession(
        SessionController.userId -> FakeMa.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe OK
    }

    "Block non restricted context invocations when student wants to reschedule his own report card entry" in new FakeApplication() {
      when(roleService.authorityFor(FakeStudent.toString)).thenReturn(Success(Some(FakeStudentAuth)))
      when(roleService.checkWith((Some(FakeCourse), reportCardEntry.update))(FakeStudentAuth)).thenReturn(Success(false))

      val request = FakeRequest(
        PUT,
        s"$FakeCourseUri/reportCards/${UUID.randomUUID}/entries/${UUID.randomUUID}",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> LwmMimeType.reportCardEntryV1Json)),
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