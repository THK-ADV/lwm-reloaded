package security

import java.util.UUID

import base.TestBaseDefinition
import controllers.SessionController
import models.Login
import models.security.{Authority, Permission, RefRole, Role}
import org.scalatest.WordSpec
import play.api.ApplicationLoader.Context
import play.api.libs.json.Json
import play.api.mvc.Results
import play.api.test.Helpers._
import play.api.test.{FakeRequest, WithApplicationLoader}
import play.api.{Application, ApplicationLoader}
import services.RoleService
import store.{UsernameResolver, Namespace, SesameRepository}
import utils.LWMActions.{SecureAction, SecureContentTypedAction}
import utils.{LwmMimeType, DefaultLwmApplication}
import org.mockito.Mockito._
import org.scalatest.mock.MockitoSugar.mock
import org.mockito.Matchers._


class SecureActionSpec extends WordSpec with TestBaseDefinition {

  implicit val roleService = mock[RoleService]

  val sufficientPermissions = Set(Permission("view"), Permission("create"), Permission("delete"))
  val insufficientPermissions = Set(Permission("view"), Permission("delete"))

  val module1 = UUID.randomUUID()
  val module2 = UUID.randomUUID()
  val role1 = Role("testRole1", sufficientPermissions)
  val role2 = Role("testRole2", insufficientPermissions)

  val module1UserRole1 = RefRole(Some(module1), role1.id, RefRole.randomUUID)
  val module1UserRole2 = RefRole(Some(module1), role2.id, RefRole.randomUUID)
  val module2UserRole2 = RefRole(Some(module2), role2.id, RefRole.randomUUID)

  val ns = Namespace("http://lwm.gm.fh-koeln.de/")
  val repository = SesameRepository(ns)
  val defaultRoleService = new RoleService(repository)
  val userId = UUID.randomUUID()
  val failedResponse = Json.obj(
    "status" -> "KO",
    "message" -> "Insufficient permissions for given action"
  )

  class WithDepsApplication extends WithApplicationLoader(new ApplicationLoader {
    override def load(context: Context): Application = new DefaultLwmApplication(context) {
      override def resolver: UsernameResolver = new UsernameResolver {
        override def resolve(systemId: String): Option[UUID] = Some(userId)
      }
    }.application
  })

  "A secured action" should {

    "apply a simple permission checking function" in new WithDepsApplication {
      when(roleService.authorityFor(anyString())).thenReturn(Some(Authority.empty))

      val action1 = SecureAction()(_ => true) { req => Results.Ok("Passed") }
      val action2 = SecureAction()(_ => false) {req => Results.Ok("Passed")}

      val request = FakeRequest("GET", "/").withSession(SessionController.userId -> userId.toString)

      val result1 = call(action1, request)
      val result2 = call(action2, request)

      status(result1) should be(OK)
      status(result2) should be(UNAUTHORIZED)
      contentAsString(result1) should be("Passed")
      contentAsJson(result2) should be(failedResponse)
    }

    "propagate an action when sufficient permissions are provided" in new WithDepsApplication {
      when(roleService.authorityFor(anyString())).thenReturn(Some(Authority(userId, Set(module1UserRole1), UUID.randomUUID())))
      when(roleService.checkWith((Some(module1), sufficientPermissions))(Set(module1UserRole1))).thenReturn(true)

      val action = SecureAction((Some(module1), sufficientPermissions)) {
        req => Results.Ok("Passed")
      }

      val request = FakeRequest("GET", "/").withSession(SessionController.userId -> userId.toString)

      val result = call(action, request)

      status(result) should be(OK)
      contentAsString(result) should be("Passed")
    }

    "block the propagation of an action when insufficient permissions are provided" in new WithDepsApplication {
      when(roleService.authorityFor(anyString())).thenReturn(Some(Authority(userId, Set(module1UserRole2), UUID.randomUUID())))
      when(roleService.checkWith((Some(module1), sufficientPermissions))(Set(module1UserRole2))).thenReturn(false)

      val action = SecureAction((Some(module1), sufficientPermissions)) {
        req => Results.Ok("Passed")
      }

      val request = FakeRequest("GET", "/").withSession(SessionController.userId -> userId.toString)

      val result = call(action, request)

      status(result) should be(UNAUTHORIZED)
      contentAsJson(result) should be(failedResponse)
    }

    "block the propagation of an action when an improper module is provided" in new WithDepsApplication {
      when(roleService.authorityFor(anyString())).thenReturn(Some(Authority(userId, Set(module2UserRole2), UUID.randomUUID())))
      when(roleService.checkWith((Some(module1), sufficientPermissions))(Set(module2UserRole2))).thenReturn(false)

      val action = SecureAction((Some(module1), sufficientPermissions)) {
        req => Results.Ok("Passed")
      }

      val request = FakeRequest("GET", "/").withSession(SessionController.userId -> userId.toString)

      val result = call(action, request)

      status(result) should be(UNAUTHORIZED)
      contentAsJson(result) should be(failedResponse)
    }

    "parse content types securely" in new WithDepsApplication {
      implicit val mimeType = LwmMimeType.loginV1Json

      when(roleService.authorityFor(anyString())).thenReturn(Some(Authority(userId, Set(module1UserRole2), UUID.randomUUID())))
      when(roleService.checkWith(anyObject())(anyObject())).thenReturn(true)

      val action = SecureContentTypedAction()(_ => true) {
        request =>
          request.body.validate[Login].fold(
            seq => {
              Results.InternalServerError("Failed to validate")
            },
            succ => {
              Results.Ok("Passed")
            }
          )
      }

      val login = Json.obj(
        "username" -> "student1",
        "password" -> "abcde123"
      )

      val request = FakeRequest("POST", "/")
        .withSession(SessionController.userId -> userId.toString)
        .withJsonBody(login)
        .withHeaders("Content-Type" -> mimeType.value)

      val result = call(action, request)

      status(result) should be(OK)
      contentAsString(result) should be("Passed")
    }
  }
}
