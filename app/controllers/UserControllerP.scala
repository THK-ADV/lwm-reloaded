package controllers

import java.util.UUID

import models.User
import play.api.mvc.{Controller, Result}
import services._
import store.Resolvers
import utils.LwmMimeType
import play.api.libs.json.{JsValue, Json}

import scala.collection.Map
import scala.concurrent.Future
import scala.util.control.NonFatal

object UserControllerP {
  lazy val statusAttribute = "status"
  lazy val atomicAttribute = "atomic"
}

class UserControllerP(val roleService: RoleService, val sessionService: SessionHandlingService, val resolvers: Resolvers, val ldapService: LdapService, val userService: UserService) extends Controller
  with Secured
  with SessionChecking
  with SecureControllerContext
  with Filterable[User]
  with ContentTyped
  with Chunked {

  import scala.concurrent.ExecutionContext.Implicits.global
  import models.User.writes

  override protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[User]) = ???

  override implicit def mimeType = LwmMimeType.userV1Json

  // /users
  // /users?status=employee
  // /users?status=lecturer
  // /users?status=student

  // TODO expand queryString to work dynamically
  def allUsers() = contextFrom(GetAll) asyncAction { request =>
    request.getQueryString(UserControllerP.statusAttribute).fold {
      handle(userService.getAll)(body => Json.toJson(body.map(_.user)))
    } { status =>
      handle(userService.filter(status))(body => Json.toJson(body.map(_.user)))
    }
  }

  private def handle[A](future: Future[A])(toJson: A => JsValue): Future[Result] = future.map(a => Ok(toJson(a))).recover { case NonFatal(e) =>
    InternalServerError(Json.obj(
      "status" -> "KO",
      "error" -> e.getMessage
    ))
  }
}
