package security

import java.util.UUID

import base.{SecurityBaseDefinition, TestBaseDefinition}
import controllers.SessionController
import models.labwork.AssignmentPlan
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

class AssignmentPlanControllerSecuritySpec extends WordSpec with TestBaseDefinition with SecurityBaseDefinition {

  "A AssignmentPlanControllerSecuritySpec " should {

    when(sessionService.isValid(Matchers.anyObject())).thenReturn(Future.successful(true))

    "Block any non restricted context invocations even when user is admin because route is not found" in new FakeApplication() {
      when(roleService.authorities(FakeAdmin)).thenReturn(Success(Set(FakeAdminAuth)))
      when(roleService.checkAuthority((None, god))(FakeAdminAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        GET,
        s"/assignmentPlans"
      ).withSession(
        SessionController.userId -> FakeAdmin.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe NOT_FOUND
    }

    "Allow restricted context invocations when admin wants to create an assignmentPlan" in new FakeApplication() {
      import models.labwork.AssignmentPlan.writes

      when(roleService.authorities(FakeAdmin)).thenReturn(Success(Set(FakeAdminAuth)))
      when(roleService.checkAuthority((Some(FakeCourse), assignmentPlan.create))(FakeAdminAuth)).thenReturn(Success(true))

      val json = Json.toJson(AssignmentPlan.empty)

      val request = FakeRequest(
        POST,
        s"$FakeCourseUri/assignmentPlans",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> LwmMimeType.assignmentPlanV1Json)),
        json
      ).withSession(
        SessionController.userId -> FakeAdmin.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe CREATED
    }

    "Allow restricted context invocations when mv wants to update an assignmentPlan" in new FakeApplication() {
      import models.labwork.AssignmentPlan.writes

      when(roleService.authorities(FakeMv)).thenReturn(Success(Set(FakeMvAuth)))
      when(roleService.checkAuthority((Some(FakeCourse), assignmentPlan.update))(FakeMvAuth)).thenReturn(Success(true))

      val json = Json.toJson(AssignmentPlan.empty)

      val request = FakeRequest(
        PUT,
        s"$FakeCourseUri/assignmentPlans/${UUID.randomUUID()}",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> LwmMimeType.assignmentPlanV1Json)),
        json
      ).withSession(
        SessionController.userId -> FakeMv.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe OK
    }

    "Allow restricted context invocations when ma wants to get an single assignmentPlan" in new FakeApplication() {
      when(roleService.authorities(FakeMa)).thenReturn(Success(Set(FakeMaAuth)))
      when(roleService.checkAuthority((Some(FakeCourse), assignmentPlan.get))(FakeMaAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        GET,
        s"$FakeCourseUri/assignmentPlans/${UUID.randomUUID()}"
      ).withSession(
        SessionController.userId -> FakeMa.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe NOT_FOUND
    }

    "Block restricted context invocations when ma wants to update an assignmentPlan" in new FakeApplication() {
      import models.labwork.AssignmentPlan.writes

      when(roleService.authorities(FakeMa)).thenReturn(Success(Set(FakeMaAuth)))
      when(roleService.checkAuthority((Some(FakeCourse), assignmentPlan.update))(FakeMaAuth)).thenReturn(Success(false))

      val json = Json.toJson(AssignmentPlan.empty)

      val request = FakeRequest(
        PUT,
        s"$FakeCourseUri/assignmentPlans/${UUID.randomUUID()}",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> LwmMimeType.assignmentPlanV1Json)),
        json
      ).withSession(
        SessionController.userId -> FakeMa.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe UNAUTHORIZED
    }
  }
}
