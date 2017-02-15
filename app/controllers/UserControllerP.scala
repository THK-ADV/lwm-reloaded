package controllers

import models.User
import play.api.mvc.{Controller, Result}
import services._
import store.Resolvers
import utils.LwmMimeType
import play.api.libs.json.{JsValue, Json}

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

object UserControllerP {
  lazy val statusAttribute = "status"
  lazy val atomicAttribute = "atomic"
}

class UserControllerP(val roleService: RoleService, val sessionService: SessionHandlingService, val resolvers: Resolvers, val ldapService: LdapService, val userService: UserService) extends Controller
  with Secured
  with SessionChecking
  with SecureControllerContext
  with ContentTyped
  with Chunked {

  import scala.concurrent.ExecutionContext.Implicits.global

  override implicit def mimeType = LwmMimeType.userV1Json

  def allUsers() = contextFrom(GetAll) asyncAction { request =>
    import controllers.UserControllerP._

    val status = request.queryString.get(statusAttribute).flatMap(_.headOption)
    val atomic = request.queryString.get(atomicAttribute).flatMap(_.headOption).map(s => Try(s.toBoolean))

    (status, atomic) match {
      case (Some(s) , Some(Success(a))) if a =>
        handle(userService.atomic(s)) { students =>
          import models.PostgresStudentAtom.writesAtom
          Json.toJson(students)
        }
      case (Some(s), None) =>
        handle(userService.filter(s)) { users =>
          import models.User.writes
          Json.toJson(users)
        }
      case (None, None) =>
        handle(userService.getAll) { dbUsers =>
          import models.User.writes
          Json.toJson(dbUsers.map(_.user))
        }
      case (None, Some(Success(_))) =>
        Future.successful(internalServerError(s"atomic only works for $statusAttribute=${User.studentType}"))
      case (_, Some(Failure(e))) =>
        Future.successful(internalServerError(e.getMessage))
    }
  }

  private def handle[A](future: Future[A])(toJson: A => JsValue): Future[Result] = future.map(a => Ok(toJson(a))).recover {
    case NonFatal(e) => internalServerError(e.getMessage)
  }

  private def internalServerError(message: String) = InternalServerError(Json.obj("status" -> "KO", "message" -> message))
}
