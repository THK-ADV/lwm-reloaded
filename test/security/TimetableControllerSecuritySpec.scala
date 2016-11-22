package security

import java.util.UUID

import base.StreamHandler._
import base.{SecurityBaseDefinition, TestBaseDefinition}
import controllers.SessionController
import models.Permissions._
import models.{Timetable, TimetableEntry}
import org.joda.time.{DateTime, LocalDate}
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

class TimetableControllerSecuritySpec extends WordSpec with TestBaseDefinition with SecurityBaseDefinition {
  
  "A TimetableControllerSecuritySpec " should {

    when(sessionService.isValid(Matchers.anyObject())).thenReturn(Future.successful(true))

    "Allow restricted context invocations when admin wants to update a timetable" in new FakeApplication() {
      import models.Timetable.writes
      
      when(roleService.authorities(FakeAdmin)).thenReturn(Success(Set(FakeAdminAuth)))
      when(roleService.checkAuthority((Some(FakeCourse), timetable.update))(FakeAdminAuth)).thenReturn(Success(true))

      val json = Json.toJson(
        Timetable(UUID.randomUUID(), Set.empty[TimetableEntry], LocalDate.now, Set.empty[DateTime])
      )

      val request = FakeRequest(
        PUT,
        s"$FakeCourseUri/timetables/${UUID.randomUUID()}",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> LwmMimeType.timetableV1Json)),
        json
      ).withSession(
        SessionController.userId -> FakeAdmin.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe OK
    }

    "Allow restricted context invocations when mv wants to create a timetable" in new FakeApplication() {
      import models.Timetable.writes

      when(roleService.authorities(FakeMv)).thenReturn(Success(Set(FakeMvAuth)))
      when(roleService.checkAuthority((Some(FakeCourse), timetable.create))(FakeMvAuth)).thenReturn(Success(true))

      val json = Json.toJson(
        Timetable(UUID.randomUUID(), Set.empty[TimetableEntry], LocalDate.now, Set.empty[DateTime])
      )

      val request = FakeRequest(
        POST,
        s"$FakeCourseUri/timetables",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> LwmMimeType.timetableV1Json)),
        json
      ).withSession(
        SessionController.userId -> FakeMv.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe CREATED
    }

    "Allow restricted context invocations when mv wants to delete a timetable" in new FakeApplication() {
      when(roleService.authorities(FakeMv)).thenReturn(Success(Set(FakeMvAuth)))
      when(roleService.checkAuthority((Some(FakeCourse), timetable.delete))(FakeMvAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        DELETE,
        s"$FakeCourseUri/timetables/${UUID.randomUUID()}"
      ).withSession(
        SessionController.userId -> FakeMv.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe OK
    }

    "Allow restricted context invocations when mv wants to get a single timetable" in new FakeApplication() {
      when(roleService.authorities(FakeMv)).thenReturn(Success(Set(FakeMvAuth)))
      when(roleService.checkAuthority((Some(FakeCourse), timetable.get))(FakeMvAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        GET,
        s"$FakeCourseUri/timetables/${UUID.randomUUID()}"
      ).withSession(
        SessionController.userId -> FakeMv.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe NOT_FOUND
    }

    "Allow restricted context invocations when ma wants to get a single timetable" in new FakeApplication() {
      when(roleService.authorities(FakeMa)).thenReturn(Success(Set(FakeMaAuth)))
      when(roleService.checkAuthority((Some(FakeCourse), timetable.get))(FakeMaAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        GET,
        s"$FakeCourseUri/timetables/${UUID.randomUUID()}"
      ).withSession(
        SessionController.userId -> FakeMa.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe NOT_FOUND
    }

    "Allow restricted context invocations when ma wants to get all timetables which belong to him" in new FakeApplication() {
      when(roleService.authorities(FakeMa)).thenReturn(Success(Set(FakeMaAuth)))
      when(roleService.checkAuthority((Some(FakeCourse), timetable.getAll))(FakeMaAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        GET,
        s"$FakeCourseUri/timetables?course=$course"
      ).withSession(
        SessionController.userId -> FakeMa.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe OK
      contentFromStream(result) shouldBe emptyJson
    }

    "Block restricted context invocations when ma wants to create timetable" in new FakeApplication() {
      import models.Timetable.writes
      
      when(roleService.authorities(FakeMa)).thenReturn(Success(Set(FakeMaAuth)))
      when(roleService.checkAuthority((Some(FakeCourse), timetable.create))(FakeMaAuth)).thenReturn(Success(false))

      val json = Json.toJson(
        Timetable(UUID.randomUUID(), Set.empty[TimetableEntry], LocalDate.now, Set.empty[DateTime])
      )

      val request = FakeRequest(
        POST,
        s"$FakeCourseUri/timetables",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> LwmMimeType.timetableV1Json)),
        json
      ).withSession(
        SessionController.userId -> FakeMa.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe UNAUTHORIZED
    }

    "Block restricted context invocations when ma wants to delete timetable" in new FakeApplication() {
      when(roleService.authorities(FakeMa)).thenReturn(Success(Set(FakeMaAuth)))
      when(roleService.checkAuthority((Some(FakeCourse), timetable.delete))(FakeMaAuth)).thenReturn(Success(false))

      val request = FakeRequest(
        DELETE,
        s"$FakeCourseUri/timetables/${UUID.randomUUID()}"
      ).withSession(
        SessionController.userId -> FakeMa.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe UNAUTHORIZED
    }

    "Block restricted context invocations when students wants to get a single timetable" in new FakeApplication() {
      when(roleService.authorities(FakeStudent)).thenReturn(Success(Set(FakeStudentAuth)))
      when(roleService.checkAuthority((Some(FakeCourse), timetable.get))(FakeStudentAuth)).thenReturn(Success(false))

      val request = FakeRequest(
        GET,
        s"$FakeCourseUri/timetables/${UUID.randomUUID()}"
      ).withSession(
        SessionController.userId -> FakeStudent.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe UNAUTHORIZED
    }

    "Block restricted context invocations when employee wants to get a single timetable" in new FakeApplication() {
      when(roleService.authorities(FakeEmployee)).thenReturn(Success(Set(FakeEmployeeAuth)))
      when(roleService.checkAuthority((Some(FakeCourse), timetable.get))(FakeEmployeeAuth)).thenReturn(Success(false))

      val request = FakeRequest(
        GET,
        s"$FakeCourseUri/timetables/${UUID.randomUUID()}"
      ).withSession(
        SessionController.userId -> FakeEmployee.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe UNAUTHORIZED
    }
  }
}
