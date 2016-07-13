package security

import java.util.UUID

import base.{SecurityBaseDefinition, TestBaseDefinition}
import controllers.SessionController
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

class AnnotationControllerSecuritySpec extends WordSpec with TestBaseDefinition with SecurityBaseDefinition {

  val json = Json.obj(
    "student" -> UUID.randomUUID,
    "labwork" -> UUID.randomUUID,
    "reportCardEntry" -> UUID.randomUUID,
    "message" -> "hello"
  )

  "A AnnotationControllerSecuritySpec " should {
    when(sessionService.isValid(Matchers.anyObject())).thenReturn(Future.successful(true))

    "Allow restricted invocations when mv wants to create an annotation" in new FakeApplication {
      when(roleService.authorityFor(FakeMv.toString)).thenReturn(Success(Some(FakeMvAuth)))
      when(roleService.checkWith((Some(FakeCourse), annotation.create))(FakeMvAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        POST,
        s"$FakeCourseUri/$FakeLabworkUri/annotations",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> LwmMimeType.annotationV1Json)),
        json
      ).withSession(
        SessionController.userId -> FakeMv.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe CREATED
    }

    "Allow restricted invocations when ma wants to create an annotation" in new FakeApplication {
      when(roleService.authorityFor(FakeMa.toString)).thenReturn(Success(Some(FakeMaAuth)))
      when(roleService.checkWith((Some(FakeCourse), annotation.create))(FakeMaAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        POST,
        s"$FakeCourseUri/$FakeLabworkUri/annotations",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> LwmMimeType.annotationV1Json)),
        json
      ).withSession(
        SessionController.userId -> FakeMa.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe CREATED
    }

    "Allow restricted invocations when ma wants to update an annotation" in new FakeApplication {
      when(roleService.authorityFor(FakeMa.toString)).thenReturn(Success(Some(FakeMaAuth)))
      when(roleService.checkWith((Some(FakeCourse), annotation.update))(FakeMaAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        PUT,
        s"$FakeCourseUri/$FakeLabworkUri/annotations/${UUID.randomUUID}",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> LwmMimeType.annotationV1Json)),
        json
      ).withSession(
        SessionController.userId -> FakeMa.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe OK
    }

    "Allow restricted invocations when mv wants to delete an annotation" in new FakeApplication {
      when(roleService.authorityFor(FakeMv.toString)).thenReturn(Success(Some(FakeMvAuth)))
      when(roleService.checkWith((Some(FakeCourse), annotation.delete))(FakeMvAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        DELETE,
        s"$FakeCourseUri/$FakeLabworkUri/annotations/${UUID.randomUUID}"
      ).withSession(
        SessionController.userId -> FakeMv.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe OK
    }

    "Allow restricted invocations when hk wants to get all annotation for his labwork" in new FakeApplication {
      when(roleService.authorityFor(FakeHk.toString)).thenReturn(Success(Some(FakeHkAuth)))
      when(roleService.checkWith((Some(FakeCourse), annotation.getAll))(FakeHkAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        GET,
        s"$FakeCourseUri/$FakeLabworkUri/annotations"
      ).withSession(
        SessionController.userId -> FakeHk.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe OK
    }

    "Block restricted invocations when student wants to get his annotations" in new FakeApplication {
      when(roleService.authorityFor(FakeStudent.toString)).thenReturn(Success(Some(FakeStudentAuth)))
      when(roleService.checkWith((Some(FakeCourse), annotation.get))(FakeStudentAuth)).thenReturn(Success(false))

      val request = FakeRequest(
        GET,
        s"$FakeCourseUri/$FakeLabworkUri/annotations/${UUID.randomUUID}"
      ).withSession(
        SessionController.userId -> FakeStudent.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe UNAUTHORIZED
    }
  }
}
