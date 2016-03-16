package controllers.security

import java.util.UUID

import base.TestBaseDefinition
import controllers.SessionController
import models.schedule.{Timetable, TimetableEntry}
import models.security.Permissions._
import models.semester.Blacklist
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

class TimetableControllerSecuritySpec extends WordSpec with TestBaseDefinition with SecurityBaseDefinition {
  
  "A TimetableControllerSecuritySpec " should {

    when(sessionService.isValid(Matchers.anyObject())).thenReturn(Future.successful(true))

    "Allow restricted context invocations when admin wants to update a timetable" in new FakeApplication() {
      import Timetable.writes
      
      when(roleService.authorityFor(FakeAdmin.toString)).thenReturn(Success(Some(FakeAdminAuth)))
      when(roleService.checkWith((Some(FakeCourse), timetable.update))(FakeAdminAuth)).thenReturn(Success(true))

      val json = Json.toJson(
        Timetable(UUID.randomUUID(), Set.empty[TimetableEntry], LocalDate.now, Blacklist.empty, UUID.randomUUID())
      )

      val request = FakeRequest(
        PUT,
        s"$FakeCourseUri/timetables/${UUID.randomUUID()}",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> LwmMimeType.timetableV1Json)),
        json
      ).withSession(SessionController.userId -> FakeAdmin.toString)

      val result = route(request).get

      status(result) shouldBe CREATED
    }

    "Allow restricted context invocations when mv wants to create a timetable" in new FakeApplication() {
      import Timetable.writes

      when(roleService.authorityFor(FakeMv.toString)).thenReturn(Success(Some(FakeMvAuth)))
      when(roleService.checkWith((Some(FakeCourse), timetable.create))(FakeMvAuth)).thenReturn(Success(true))

      val json = Json.toJson(
        Timetable(UUID.randomUUID(), Set.empty[TimetableEntry], LocalDate.now, Blacklist.empty, UUID.randomUUID())
      )

      val request = FakeRequest(
        POST,
        s"$FakeCourseUri/timetables",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> LwmMimeType.timetableV1Json)),
        json
      ).withSession(SessionController.userId -> FakeMv.toString)

      val result = route(request).get

      status(result) shouldBe CREATED
    }

    "Allow restricted context invocations when mv wants to delete a timetable" in new FakeApplication() {
      when(roleService.authorityFor(FakeMv.toString)).thenReturn(Success(Some(FakeMvAuth)))
      when(roleService.checkWith((Some(FakeCourse), timetable.delete))(FakeMvAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        DELETE,
        s"$FakeCourseUri/timetables/${UUID.randomUUID()}"
      ).withSession(SessionController.userId -> FakeMv.toString)

      val result = route(request).get

      status(result) shouldBe OK
    }

    "Allow restricted context invocations when mv wants to get a single timetable" in new FakeApplication() {
      when(roleService.authorityFor(FakeMv.toString)).thenReturn(Success(Some(FakeMvAuth)))
      when(roleService.checkWith((Some(FakeCourse), timetable.get))(FakeMvAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        GET,
        s"$FakeCourseUri/timetables/${UUID.randomUUID()}"
      ).withSession(SessionController.userId -> FakeMv.toString)

      val result = route(request).get

      status(result) shouldBe NOT_FOUND
    }

    "Allow restricted context invocations when ma wants to get a single timetable" in new FakeApplication() {
      when(roleService.authorityFor(FakeMa.toString)).thenReturn(Success(Some(FakeMaAuth)))
      when(roleService.checkWith((Some(FakeCourse), timetable.get))(FakeMaAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        GET,
        s"$FakeCourseUri/timetables/${UUID.randomUUID()}"
      ).withSession(SessionController.userId -> FakeMa.toString)

      val result = route(request).get

      status(result) shouldBe NOT_FOUND
    }

    "Allow restricted context invocations when ma wants to get all timetables which belongs to him" in new FakeApplication() {
      import Timetable.writes

      when(roleService.authorityFor(FakeMa.toString)).thenReturn(Success(Some(FakeMaAuth)))
      when(roleService.checkWith((Some(FakeCourse), timetable.getAll))(FakeMaAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        GET,
        s"$FakeCourseUri/timetables?course=$course"
      ).withSession(SessionController.userId -> FakeMa.toString)

      val result = route(request).get

      status(result) shouldBe OK
      contentAsJson(result) shouldBe Json.toJson(Set.empty[Timetable])
    }

    "Block restricted context invocations when ma wants to create timetable" in new FakeApplication() {
      import Timetable.writes
      
      when(roleService.authorityFor(FakeMa.toString)).thenReturn(Success(Some(FakeMaAuth)))
      when(roleService.checkWith((Some(FakeCourse), timetable.create))(FakeMaAuth)).thenReturn(Success(false))

      val json = Json.toJson(
        Timetable(UUID.randomUUID(), Set.empty[TimetableEntry], LocalDate.now, Blacklist.empty, UUID.randomUUID())
      )

      val request = FakeRequest(
        POST,
        s"$FakeCourseUri/timetables",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> LwmMimeType.timetableV1Json)),
        json
      ).withSession(SessionController.userId -> FakeMa.toString)

      val result = route(request).get

      status(result) shouldBe UNAUTHORIZED
    }

    "Block restricted context invocations when ma wants to delete timetable" in new FakeApplication() {
      when(roleService.authorityFor(FakeMa.toString)).thenReturn(Success(Some(FakeMaAuth)))
      when(roleService.checkWith((Some(FakeCourse), timetable.delete))(FakeMaAuth)).thenReturn(Success(false))

      val request = FakeRequest(
        DELETE,
        s"$FakeCourseUri/timetables/${UUID.randomUUID()}"
      ).withSession(SessionController.userId -> FakeMa.toString)

      val result = route(request).get

      status(result) shouldBe UNAUTHORIZED
    }

    "Block restricted context invocations when students wants to get a single timetable" in new FakeApplication() {
      when(roleService.authorityFor(FakeStudent.toString)).thenReturn(Success(Some(FakeStudentAuth)))
      when(roleService.checkWith((Some(FakeCourse), timetable.get))(FakeStudentAuth)).thenReturn(Success(false))

      val request = FakeRequest(
        GET,
        s"$FakeCourseUri/timetables/${UUID.randomUUID()}"
      ).withSession(SessionController.userId -> FakeStudent.toString)

      val result = route(request).get

      status(result) shouldBe UNAUTHORIZED
    }

    "Block restricted context invocations when employee wants to get a single timetable" in new FakeApplication() {
      when(roleService.authorityFor(FakeEmployee.toString)).thenReturn(Success(Some(FakeEmployeeAuth)))
      when(roleService.checkWith((Some(FakeCourse), timetable.get))(FakeEmployeeAuth)).thenReturn(Success(false))

      val request = FakeRequest(
        GET,
        s"$FakeCourseUri/timetables/${UUID.randomUUID()}"
      ).withSession(SessionController.userId -> FakeEmployee.toString)

      val result = route(request).get

      status(result) shouldBe UNAUTHORIZED
    }
  }
}
