package controllers.security

import java.util.UUID

import base.TestBaseDefinition
import controllers.SessionController
import models.applications.LabworkApplication
import models.security.Permissions._
import org.mockito.Mockito._
import org.scalatest.WordSpec
import play.api.http.HeaderNames
import play.api.libs.json.Json
import play.api.test.Helpers._
import play.api.test.{FakeHeaders, FakeRequest}
import utils.LwmMimeType

import scala.util.Success


class LabworkApplicationControllerSecuritySpec extends WordSpec with TestBaseDefinition with SecurityBaseDefinition { 
  
  "A LabworkApplicationControllerSecuritySpec " should {

    "Allow non restricted context invocations when admin wants to get all labwork applications" in new FakeApplication() {
      import LabworkApplication.writes
      
      when(roleService.authorityFor(FakeAdmin.toString)).thenReturn(Success(Some(FakeAdminAuth)))
      when(roleService.checkWith((None, labworkApplication.create))(FakeAdminAuth)).thenReturn(Success(true))

      val json = Json.toJson(
        LabworkApplication(UUID.randomUUID(), UUID.randomUUID(), Set(UUID.randomUUID(), UUID.randomUUID()))
      )

      val request = FakeRequest(
        POST,
        s"/labworkApplications",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> LwmMimeType.labworkApplicationV1Json)),
        json
      ).withSession(SessionController.userId -> FakeAdmin.toString)

      val result = route(request).get

      status(result) shouldBe CREATED
    }

    "Allow non restricted context invocations when admin wants to delete an labwork applications" in new FakeApplication() {
      when(roleService.authorityFor(FakeAdmin.toString)).thenReturn(Success(Some(FakeAdminAuth)))
      when(roleService.checkWith((None, labworkApplication.delete))(FakeAdminAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        DELETE,
        s"/labworkApplications/${UUID.randomUUID()}"
      ).withSession(SessionController.userId -> FakeAdmin.toString)

      val result = route(request).get

      status(result) shouldBe OK
    }

    "Allow non restricted context invocations when student wants to create an labwork applications" in new FakeApplication() {
      import LabworkApplication.writes

      when(roleService.authorityFor(FakeStudent.toString)).thenReturn(Success(Some(FakeStudentAuth)))
      when(roleService.checkWith((None, labworkApplication.create))(FakeStudentAuth)).thenReturn(Success(true))

      val json = Json.toJson(
        LabworkApplication(UUID.randomUUID(), UUID.randomUUID(), Set(UUID.randomUUID(), UUID.randomUUID()))
      )

      val request = FakeRequest(
        POST,
        s"/labworkApplications",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> LwmMimeType.labworkApplicationV1Json)),
        json
      ).withSession(SessionController.userId -> FakeStudent.toString)

      val result = route(request).get

      status(result) shouldBe CREATED
    }

    "Allow non restricted context invocations when student wants to delete an labwork applications" in new FakeApplication() {
      when(roleService.authorityFor(FakeStudent.toString)).thenReturn(Success(Some(FakeStudentAuth)))
      when(roleService.checkWith((None, labworkApplication.delete))(FakeStudentAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        DELETE,
        s"/labworkApplications/${UUID.randomUUID()}"
      ).withSession(SessionController.userId -> FakeStudent.toString)

      val result = route(request).get

      status(result) shouldBe OK
    }

    "Block non restricted context invocations when student wants to get all labwork applications" in new FakeApplication() {
      when(roleService.authorityFor(FakeStudent.toString)).thenReturn(Success(Some(FakeStudentAuth)))
      when(roleService.checkWith((None, labworkApplication.getAll))(FakeStudentAuth)).thenReturn(Success(false))

      val request = FakeRequest(
        GET,
        s"/labworkApplications"
      ).withSession(SessionController.userId -> FakeStudent.toString)

      val result = route(request).get

      status(result) shouldBe UNAUTHORIZED
    }

    "Block non restricted context invocations when ma wants to get all labwork applications" in new FakeApplication() {
      when(roleService.authorityFor(FakeMa.toString)).thenReturn(Success(Some(FakeMaAuth)))
      when(roleService.checkWith((None, labworkApplication.getAll))(FakeMaAuth)).thenReturn(Success(false))

      val request = FakeRequest(
        GET,
        s"/labworkApplications"
      ).withSession(SessionController.userId -> FakeMa.toString)

      val result = route(request).get

      status(result) shouldBe UNAUTHORIZED
    }

    "Block non restricted context invocations when ma wants to create an labwork applications" in new FakeApplication() {
      import LabworkApplication.writes

      when(roleService.authorityFor(FakeMa.toString)).thenReturn(Success(Some(FakeMaAuth)))
      when(roleService.checkWith((None, labworkApplication.create))(FakeMaAuth)).thenReturn(Success(false))

      val json = Json.toJson(
        LabworkApplication(UUID.randomUUID(), UUID.randomUUID(), Set(UUID.randomUUID(), UUID.randomUUID()))
      )

      val request = FakeRequest(
        POST,
        s"/labworkApplications",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> LwmMimeType.labworkApplicationV1Json)),
        json
      ).withSession(SessionController.userId -> FakeMa.toString)

      val result = route(request).get

      status(result) shouldBe UNAUTHORIZED
    }
  }
}
