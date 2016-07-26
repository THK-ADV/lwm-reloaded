package security

import java.util.UUID

import base.TestBaseDefinition
import controllers.SessionController
import models.Login
import models.security.{Authority, Permission, RefRole, Role}
import models.users.User
import org.scalatest.WordSpec
import org.w3.banana.PointedGraph
import play.api.ApplicationLoader.Context
import play.api.libs.json.Json
import play.api.mvc.Results
import play.api.test.Helpers._
import play.api.test.{FakeRequest, WithApplicationLoader}
import play.api.{Application, ApplicationLoader}
import services.{RoleService, SessionHandlingService}
import store.{Namespace, Resolvers, SesameRepository}
import utils.LwmActions.{SecureAction, SecureContentTypedAction}
import utils.{DefaultLwmApplication, LwmMimeType}
import org.mockito.Mockito._
import org.scalatest.mock.MockitoSugar.mock
import org.mockito.Matchers._

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

//TODO: Add another test checking if the propagation is stopped when authorities cannot be found and such
class SecureActionSpec extends WordSpec with TestBaseDefinition {

  implicit val roleService = mock[RoleService]
  implicit val sessionService = mock[SessionHandlingService]

  val sufficientPermissions = Set(Permission("view"), Permission("create"), Permission("delete"))
  val insufficientPermissions = Set(Permission("view"), Permission("delete"))

  val module1 = UUID.randomUUID()
  val module2 = UUID.randomUUID()
  val role1 = Role("testRole1", sufficientPermissions)
  val role2 = Role("testRole2", insufficientPermissions)

  val module1UserRole1 = RefRole(Some(module1), role1.id)
  val module1UserRole2 = RefRole(Some(module1), role2.id)
  val module2UserRole2 = RefRole(Some(module2), role2.id)

  val ns = Namespace("http://lwm.gm.fh-koeln.de/")
  val repository = SesameRepository(ns)
  val defaultRoleService = new RoleService(repository)
  val userId = UUID.randomUUID()
  val failedResponse = Json.obj(
    "status" -> "KO",
    "message" -> "Insufficient permissions for given action"
  )

  def authority(refRoles: Set[RefRole]): Authority = Authority(UUID.randomUUID(), refRoles map (_.id))

  class WithDepsApplication extends WithApplicationLoader(new ApplicationLoader {
    override def load(context: Context): Application = new DefaultLwmApplication(context) {
      override val resolvers: Resolvers = new Resolvers {
        override def username(systemId: String): Try[Option[UUID]] = Success(Some(userId))

        override type R = Nothing

        override def missingUserData[A <: User](v: A): Try[PointedGraph[R]] = Failure(new Throwable("Not invoked"))

        override def degree(abbreviation: String): Try[UUID] = Failure(new Throwable("Not invoked"))
      }
    }.application
  })

  "A secured action" should {

    "propagate an action when sufficient permissions are provided" in new WithDepsApplication {
      val auth = Authority(userId, Set(module1UserRole1.id))

      when(roleService.authorityFor(anyString())).thenReturn(Success(Some(auth)))
      when(roleService.checkWith((Some(module1), sufficientPermissions.head))(auth)).thenReturn(Success(true))
      when(sessionService.isValid(anyObject())).thenReturn(Future.successful(true))

      val action = SecureAction((Some(module1), sufficientPermissions.head)) {
        req => Results.Ok("Passed")
      }

      val request = FakeRequest("GET", "/").withSession(
        SessionController.userId -> userId.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = call(action, request)

      status(result) shouldBe OK
      contentAsString(result) shouldBe "Passed"
    }

    "block an action when no user-id has been found" in new WithDepsApplication {
      val auth = Authority(userId, Set(module1UserRole1.id))
      val response = Json.obj(
        "status" -> "KO",
        "message" -> "No user-id found in session"
      )

      val action = SecureAction((Some(module1), sufficientPermissions.head)) {
        req => Results.Ok("Passed")
      }

      val request = FakeRequest("GET", "/").withSession(SessionController.sessionId -> UUID.randomUUID.toString)

      val result = call(action, request)

      status(result) shouldBe UNAUTHORIZED
      contentAsJson(result) shouldBe response
    }

    "block the propagation of an action when no valid session has been found" in new WithDepsApplication {
      val auth = Authority(userId, Set(module1UserRole2.id))
      val response = Json.obj(
        "status" -> "KO",
        "message" -> "No session-id found in session"
      )
      when(sessionService.isValid(anyObject())).thenReturn(Future.successful(false))

      val action = SecureAction((Some(module1), sufficientPermissions.head)) {
        req => Results.Ok("Passed")
      }

      val request = FakeRequest("GET", "/").withSession(SessionController.userId -> userId.toString)

      val result = call(action, request)

      status(result) shouldBe UNAUTHORIZED
      contentAsJson(result) shouldBe response

    }

    "block the propagation of an action when insufficient permissions are provided" in new WithDepsApplication {
      val auth = Authority(userId, Set(module1UserRole2.id))

      when(roleService.authorityFor(anyString())).thenReturn(Success(Some(auth)))
      when(roleService.checkWith((Some(module1), sufficientPermissions.head))(auth)).thenReturn(Success(false))
      when(sessionService.isValid(anyObject())).thenReturn(Future.successful(true))

      val action = SecureAction((Some(module1), sufficientPermissions.head)) {
        req => Results.Ok("Passed")
      }

      val request = FakeRequest("GET", "/").withSession(
        SessionController.userId -> userId.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = call(action, request)

      status(result) shouldBe UNAUTHORIZED
      contentAsJson(result) shouldBe failedResponse
    }

    "block the propagation of an action when an improper module is provided" in new WithDepsApplication {
      val auth = Authority(userId, Set(module2UserRole2.id))

      when(roleService.authorityFor(anyString())).thenReturn(Success(Some(auth)))
      when(roleService.checkWith((Some(module1), sufficientPermissions.head))(auth)).thenReturn(Success(false))
      when(sessionService.isValid(anyObject())).thenReturn(Future.successful(true))

      val action = SecureAction((Some(module1), sufficientPermissions.head)) {
        req => Results.Ok("Passed")
      }

      val request = FakeRequest("GET", "/").withSession(
        SessionController.userId -> userId.toString,
        SessionController.sessionId -> UUID.randomUUID.toString
      )

      val result = call(action, request)

      status(result) shouldBe UNAUTHORIZED
      contentAsJson(result) shouldBe failedResponse
    }

    "parse content types securely" in new WithDepsApplication {
      implicit val mimeType = LwmMimeType.loginV1Json

      val perm = Permission("No permission")
      when(roleService.authorityFor(anyString())).thenReturn(Success(Some(Authority(userId, Set(module1UserRole2.id)))))
      when(roleService.checkWith(anyObject())(anyObject())).thenReturn(Success(true))
      when(sessionService.isValid(anyObject())).thenReturn(Future.successful(true))

      val action = SecureContentTypedAction((None, perm)) {
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
        .withSession(
          SessionController.userId -> userId.toString,
          SessionController.sessionId -> UUID.randomUUID.toString)
        .withJsonBody(login)
        .withHeaders("Content-Type" -> mimeType.value)

      val result = call(action, request)

      status(result) shouldBe OK
      contentAsString(result) shouldBe "Passed"
    }
  }
}
