package security

import java.util.UUID

import base.{SecurityBaseDefinition, TestBaseDefinition}
import controllers.SessionController
import models.labwork.Group
import models.security.Permissions._
import org.mockito.Matchers
import org.mockito.Mockito._
import org.scalatest.WordSpec
import play.api.test.Helpers._
import play.api.test.FakeRequest

import scala.concurrent.Future
import scala.util.Success

class GroupControllerSecuritySpec extends WordSpec with TestBaseDefinition with SecurityBaseDefinition { 

  "A GroupControllerSecuritySpec " should {

    when(sessionService.isValid(Matchers.anyObject())).thenReturn(Future.successful(true))
    when(groupService.groupBy(Matchers.anyObject(), Matchers.anyObject())).thenReturn(Success(Set.empty[Group]))

    "Allow restricted invocations when mv wants to preview a group" in new FakeApplication() {
      when(roleService.authorities(FakeMv)).thenReturn(Success(Set(FakeMvAuth)))
      when(roleService.checkAuthority((Some(FakeCourse), group.create))(FakeMvAuth)).thenReturn(Success(true))

      val request = FakeRequest(
        GET,
        s"$FakeCourseUri/labworks/${UUID.randomUUID()}/groups/preview?count=10"
      ).withSession(
        SessionController.userId -> FakeMv.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = route(request).get

      status(result) shouldBe OK
    }
  }
}
