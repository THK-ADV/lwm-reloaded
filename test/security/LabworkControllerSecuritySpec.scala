package security

import java.util.UUID

import base.StreamHandler._
import base.{SecurityBaseDefinition, TestBaseDefinition}
import controllers.SessionController
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

class LabworkControllerSecuritySpec extends WordSpec with TestBaseDefinition with SecurityBaseDefinition {

  "A LabworkControllerSecuritySpec " should {

    when(sessionService.isValid(Matchers.anyObject())).thenReturn(Future.successful(true))

    "Allow non restricted context invocations when user is an admin" in new FakeApplication() {
      when(roleService.authorities(FakeAdmin)).thenReturn(Success(Set(FakeAdminAuth)))
      when(roleService.checkAuthority((Some(FakeCourse), labwork.getAll))(FakeAdminAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        GET,
        s"$FakeCourseUri/labworks"
      ).withSession(
        SessionController.userId -> FakeAdmin.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe OK
    }

    "Allow restricted invocations when admin wants to update a labwork" in new FakeApplication() {
      when(roleService.authorities(FakeAdmin)).thenReturn(Success(Set(FakeAdminAuth)))
      when(roleService.checkAuthority((Some(FakeCourse), labwork.update))(FakeAdminAuth)).thenReturn(Success(true))

      val json = Json.obj(
        "label" -> "",
        "description" -> "",
        "semester" -> UUID.randomUUID(),
        "course" -> UUID.randomUUID(),
        "degree" -> UUID.randomUUID(),
        "subscribable" -> false,
        "published" -> false
      )

      val request = FakeRequest(
        PUT,
        s"$FakeCourseUri/labworks/${UUID.randomUUID()}",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> LwmMimeType.labworkV1Json)),
        json
      ).withSession(
        SessionController.userId -> FakeAdmin.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe OK
    }

    "Allow restricted invocations when admin wants to delete a labwork" in new FakeApplication() {
      when(roleService.authorities(FakeAdmin)).thenReturn(Success(Set(FakeAdminAuth)))
      when(roleService.checkAuthority((None, prime))(FakeAdminAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        DELETE,
        s"$FakeCourseUri/labworks/${UUID.randomUUID()}"
      ).withSession(
        SessionController.userId -> FakeAdmin.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe OK
    }

    "Block restricted invocations when mv wants to delete a labwork" in new FakeApplication() {
      when(roleService.authorities(FakeMv)).thenReturn(Success(Set(FakeMvAuth)))
      when(roleService.checkAuthority((None, prime))(FakeMvAuth)).thenReturn(Success(false))

      val request = FakeRequest(
        DELETE,
        s"$FakeCourseUri/labworks/${UUID.randomUUID()}"
      ).withSession(
        SessionController.userId -> FakeMv.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe UNAUTHORIZED
    }

    "Allow restricted invocations when mv wants to create a labwork" in new FakeApplication() {
      when(roleService.authorities(FakeMv)).thenReturn(Success(Set(FakeMvAuth)))
      when(roleService.checkAuthority((Some(FakeCourse), labwork.create))(FakeMvAuth)).thenReturn(Success(true))

      val json = Json.obj(
        "label" -> "",
        "description" -> "",
        "semester" -> UUID.randomUUID(),
        "course" -> UUID.randomUUID(),
        "degree" -> UUID.randomUUID(),
        "subscribable" -> false,
        "published" -> false
      )

      val request = FakeRequest(
        POST,
        s"$FakeCourseUri/labworks",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> LwmMimeType.labworkV1Json)),
        json
      ).withSession(
        SessionController.userId -> FakeMv.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe CREATED
    }

    "Allow restricted invocations when mv wants to get a single labwork" in new FakeApplication() {
      when(roleService.authorities(FakeMv)).thenReturn(Success(Set(FakeMvAuth)))
      when(roleService.checkAuthority((Some(FakeCourse), labwork.get))(FakeMvAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        GET,
        s"$FakeCourseUri/labworks/${UUID.randomUUID()}"
      ).withSession(
        SessionController.userId -> FakeMv.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe NOT_FOUND
    }

    "Allow restricted invocations when mv wants to get a all labworks which belongs to him" in new FakeApplication() {
      when(roleService.authorities(FakeMv)).thenReturn(Success(Set(FakeMvAuth)))
      when(roleService.checkAuthority((Some(FakeCourse), labwork.getAll))(FakeMvAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        GET,
        s"$FakeCourseUri/labworks?course=$FakeCourse"
      ).withSession(
        SessionController.userId -> FakeMv.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe OK
      contentFromStream(result) shouldBe emptyJson
    }

    "Allow restricted invocations when mv wants to update a labwork which belongs to him" in new FakeApplication() {
      when(roleService.authorities(FakeMv)).thenReturn(Success(Set(FakeMvAuth)))
      when(roleService.checkAuthority((Some(FakeCourse), labwork.update))(FakeMvAuth)).thenReturn(Success(true))

      val json = Json.obj(
        "label" -> "",
        "description" -> "",
        "semester" -> UUID.randomUUID(),
        "course" -> UUID.randomUUID(),
        "degree" -> UUID.randomUUID(),
        "subscribable" -> false,
        "published" -> false
      )

      val request = FakeRequest(
        PUT,
        s"$FakeCourseUri/labworks/${UUID.randomUUID()}",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> LwmMimeType.labworkV1Json)),
        json
      ).withSession(
        SessionController.userId -> FakeMv.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe OK
    }

    "Allow restricted invocations when ma wants to get a single labwork" in new FakeApplication() {
      when(roleService.authorities(FakeMa)).thenReturn(Success(Set(FakeMaAuth)))
      when(roleService.checkAuthority((Some(FakeCourse), labwork.get))(FakeMaAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        GET,
        s"$FakeCourseUri/labworks/${UUID.randomUUID()}"
      ).withSession(
        SessionController.userId -> FakeMa.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe NOT_FOUND
    }

    "Allow restricted invocations when ma wants to get all labwork which belongs to him" in new FakeApplication() {
      when(roleService.authorities(FakeMa)).thenReturn(Success(Set(FakeMaAuth)))
      when(roleService.checkAuthority((Some(FakeCourse), labwork.getAll))(FakeMaAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        GET,
        s"$FakeCourseUri/labworks?course=$FakeCourse"
      ).withSession(
        SessionController.userId -> FakeMa.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe OK
      contentFromStream(result) shouldBe emptyJson
    }

    "Allow restricted invocations when hk wants to get a single labwork " in new FakeApplication() {
      when(roleService.authorities(FakeHk)).thenReturn(Success(Set(FakeHkAuth)))
      when(roleService.checkAuthority((Some(FakeCourse), labwork.get))(FakeHkAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        GET,
        s"$FakeCourseUri/labworks/${UUID.randomUUID()}"
      ).withSession(
        SessionController.userId -> FakeHk.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe NOT_FOUND
    }

    "Block restricted invocations when hk wants to get all labwork which belongs to him" in new FakeApplication() {
      when(roleService.authorities(FakeHk)).thenReturn(Success(Set(FakeHkAuth)))
      when(roleService.checkAuthority((Some(FakeCourse), labwork.getAll))(FakeHkAuth)).thenReturn(Success(false))

      val request = FakeRequest(
        GET,
        s"$FakeCourseUri/labworks?course=$FakeCourse"
      ).withSession(
        SessionController.userId -> FakeHk.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe UNAUTHORIZED
    }

    "Allow non restricted invocations when student wants to get a single labwork" in new FakeApplication() {
      when(roleService.authorities(FakeStudent)).thenReturn(Success(Set(FakeStudentAuth)))
      when(roleService.checkAuthority((None, labwork.get))(FakeStudentAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        GET,
        s"/labworks/${UUID.randomUUID()}"
      ).withSession(
        SessionController.userId -> FakeStudent.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe NOT_FOUND
    }

    "Allow non restricted invocations when student wants to get all labworks where he can apply for" in new FakeApplication() {
      when(roleService.authorities(FakeStudent)).thenReturn(Success(Set(FakeStudentAuth)))
      when(roleService.checkAuthority((None, labwork.getAll))(FakeStudentAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        GET,
        s"/labworks/degrees/${UUID.randomUUID}"
      ).withSession(
        SessionController.userId -> FakeStudent.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe OK
      contentFromStream(result) shouldBe emptyJson
    }

    "Block non restricted invocations when employee wants to get a single labwork" in new FakeApplication {
      when(roleService.authorities(FakeEmployee)).thenReturn(Success(Set(FakeEmployeeAuth)))
      when(roleService.checkAuthority((None, labwork.get))(FakeEmployeeAuth)).thenReturn(Success(false))

      val request = FakeRequest(
        GET,
        s"/labworks/${UUID.randomUUID()}"
      ).withSession(
        SessionController.userId -> FakeEmployee.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe UNAUTHORIZED
    }
  }
}
