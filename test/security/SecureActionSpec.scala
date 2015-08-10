package security

import java.util.UUID
import base.TestBaseDefinition
import models.Login
import org.scalatest.WordSpec
import play.api.libs.json.Json
import play.api.{Application, ApplicationLoader}
import play.api.ApplicationLoader.Context
import play.api.mvc.{Security, Results}
import play.api.test.{WithApplicationLoader, FakeRequest}
import play.api.test.Helpers._
import services.{RoleService, RoleServiceLike}
import utils.{LWMMimeType, DefaultLwmApplication}
import utils.LWMActions.{SecureContentTypedAction, SecureAction}

class SecureActionSpec extends WordSpec with TestBaseDefinition {

  val sufficientPermissions = Set(Permission("view"), Permission("create"), Permission("delete"))
  val insufficientPermissions = Set(Permission("view"), Permission("delete"))

  val module1 = UUID.randomUUID()
  val module2 = UUID.randomUUID()
  val role1 = Role("testRole1", sufficientPermissions)
  val role2 = Role("testRole2", insufficientPermissions)

  val module1UserRole1 = RefRole(Some(module1), role1)
  val module1UserRole2 = RefRole(Some(module1), role2)
  val module2UserRole2 = RefRole(Some(module2), role2)

  val defaultRoleService = new RoleService()

  class WithDepsApplication extends WithApplicationLoader(new ApplicationLoader {
    override def load(context: Context): Application = new DefaultLwmApplication(context).application
  })

  "A secured action" should {

    "apply a simple permission checking function" in {
      implicit val roleService = new RoleServiceLike[RefRole] {
        override def permissionsFor(systemId: String): Set[RefRole] = Set()

        override def checkWith(checkee: Set[RefRole])(checker: Set[RefRole]): Boolean = defaultRoleService.checkWith(checkee)(checker)
      }

      val action1 = SecureAction[RefRole]()(_ => true) { req => Results.Ok("Passed") }
      val action2 = SecureAction[RefRole]()(_ => false) {req => Results.Ok("Passed")}

      val request = FakeRequest("GET", "/")

      val result1 = call(action1, request)
      val result2 = call(action2, request)

      status(result1) should be(OK)
      status(result2) should be(UNAUTHORIZED)
      contentAsString(result1) should be("Passed")
      contentAsString(result2) should be("Insufficient permissions for given action")
    }

    "propagate an action when sufficient permissions are provided" in new WithDepsApplication {

      implicit val roleService = new RoleServiceLike[RefRole] {
        override def permissionsFor(systemId: String): Set[RefRole] = Set(module1UserRole1)

        override def checkWith(checkee: Set[RefRole])(checker: Set[RefRole]): Boolean = defaultRoleService.checkWith(checkee)(checker)
      }

      val action = SecureAction[RefRole](Set(module1UserRole1)) {
        req => Results.Ok("Passed")
      }

      val request = FakeRequest("GET", "/").withSession(Security.username -> "bla")

      val result = call(action, request)

      status(result) should be(OK)
      contentAsString(result) should be("Passed")
    }

    "block the propagation of an action when insufficient permissions are provided" in new WithDepsApplication {

      implicit val roleService = new RoleServiceLike[RefRole] {
        override def permissionsFor(systemId: String): Set[RefRole] = Set(module1UserRole2)

        override def checkWith(checkee: Set[RefRole])(checker: Set[RefRole]): Boolean = defaultRoleService.checkWith(checkee)(checker)
      }

      val action = SecureAction[RefRole](Set(module1UserRole1)) {
        req => Results.Ok("Passed")
      }

      val request = FakeRequest("GET", "/").withSession(Security.username -> "blabla")

      val result = call(action, request)

      status(result) should be(UNAUTHORIZED)
      contentAsString(result) should be("Insufficient permissions for given action")
    }

    "block the propagation of an action when an improper module is provided" in new WithDepsApplication {

      implicit val roleService = new RoleServiceLike[RefRole] {
        override def permissionsFor(systemId: String): Set[RefRole] = Set(module2UserRole2)

        override def checkWith(checkee: Set[RefRole])(checker: Set[RefRole]): Boolean = defaultRoleService.checkWith(checkee)(checker)
      }

      val action = SecureAction[RefRole](Set(module1UserRole1)) {
        req => Results.Ok("Passed")
      }

      val request = FakeRequest("GET", "/").withSession(Security.username -> "blabla")

      val result = call(action, request)

      status(result) should be(UNAUTHORIZED)
      contentAsString(result) should be("Insufficient permissions for given action")
    }

    "parse content types securely" in new WithDepsApplication {
      implicit val mimeType = LWMMimeType.loginV1Json

      implicit val roleService = new RoleServiceLike[RefRole] {
        override def permissionsFor(systemId: String): Set[RefRole] = Set(module1UserRole2)

        override def checkWith(checkee: Set[RefRole])(checker: Set[RefRole]): Boolean = defaultRoleService.checkWith(checkee)(checker)
      }

      val action = SecureContentTypedAction[RefRole]()(_ => true) {
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
        .withSession(Security.username -> "blabla")
        .withJsonBody(login)
        .withHeaders("Content-Type" -> mimeType.value)

      val result = call(action, request)

      status(result) should be(OK)
      contentAsString(result) should be("Passed")
    }
  }
}
