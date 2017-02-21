package controllers

import java.util.UUID

import models.Permissions
import play.api.mvc.{Controller, Result}
import services._
import store.{Resolvers, TableFilter, UserTable}
import utils.LwmMimeType
import play.api.libs.json.{JsValue, Json, Writes}

import scala.concurrent.Future
import scala.util.{Failure, Try}
import scala.util.control.NonFatal

object UserControllerPostgres {
  lazy val statusAttribute = "status"
  lazy val degreeAttribute = "degree"
  lazy val systemIdAttribute = "systemId"
  lazy val firstnameAttribute = "firstname"
  lazy val lastnameAttribute = "lastname"

  lazy val atomicAttribute = "atomic"
}

class UserControllerPostgres(val roleService: RoleService, val sessionService: SessionHandlingService, val resolvers: Resolvers, val ldapService: LdapService, val userService: UserService) extends Controller
  with Secured
  with SessionChecking
  with SecureControllerContext
  with ContentTyped
  with Chunked {

  import scala.concurrent.ExecutionContext.Implicits.global

  object PostgresResult {
    private def internalServerError(message: String) = InternalServerError(Json.obj("status" -> "KO", "message" -> message))
    private def notFound(element: String) = NotFound(Json.obj("status" -> "KO", "message" -> s"No such element for $element"))

    implicit class SequenceResult[A](val future: Future[Seq[A]]) {
      def jsonResult(implicit writes: Writes[A]) = future.map(a => Ok(Json.toJson(a))).recover {
        case NonFatal(e) => internalServerError(e.getMessage)
      }
    }

    implicit class OptionResult[A](val future: Future[Option[A]]) {
      def jsonResult(idForMessage: String)(implicit writes: Writes[A]) = future.map { maybeA =>
        maybeA.fold(notFound(idForMessage))(a => Ok(Json.toJson(a)) )
      }.recover {
        case NonFatal(e) => internalServerError(e.getMessage)
      }
    }
  }

  import scala.concurrent.ExecutionContext.Implicits.global
  import PostgresResult._

  override implicit def mimeType = LwmMimeType.userV1Json
  import models.User.writes

  type QueryString = Map[String, Seq[String]]

  def extractAtomic(queryString: QueryString): (QueryString, Boolean) = {
    import controllers.UserControllerPostgres.atomicAttribute

    queryString.find(_._1 == `atomicAttribute`) match {
      case Some(q) =>
        val atomic = q._2.headOption.flatMap(s => Try(s.toBoolean).toOption).fold(false)(_ == true)
        val remaining = queryString - `atomicAttribute`

        (remaining, atomic)
      case None =>
        (queryString, false)
    }
  }

  def allUsers() = contextFrom(GetAll) asyncAction { request =>
    import controllers.UserControllerPostgres._

    val (queryString, atomic) = extractAtomic(request.queryString)

    val userFilter = queryString.foldLeft(Try(List.empty[TableFilter[UserTable]])) {
      case (list, (`statusAttribute`, status)) => list.map(_.+:(UserStatusFilter(status.head)))
      case (list, (`systemIdAttribute`, systemId)) => list.map(_.+:(UserSystemIdFilter(systemId.head)))
      case (list, (`lastnameAttribute`, lastname)) => list.map(_.+:(UserLastnameFilter(lastname.head)))
      case (list, (`firstnameAttribute`, firstname)) => list.map(_.+:(UserFirstnameFilter(firstname.head)))
      case (list, (`degreeAttribute`, degree)) => list.map(_.+:(UserDegreeFilter(degree.head)))
      case _ => Failure(new Throwable("Unknown attribute"))
    }

    (for {
      filter <- Future.fromTry(userFilter)
      users <- userService.get(filter, atomic)
    } yield users).jsonResult
  }

  def user(id: String) = contextFrom(Get) asyncAction { request =>
    val atomic = extractAtomic(request.queryString)._2

    userService.get(List(UserIdFilter(id)), atomic).map(_.headOption).jsonResult(id)
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(Permissions.user.get)
    case GetAll => PartialSecureBlock(Permissions.user.getAll)
    case Create => PartialSecureBlock(Permissions.prime)
    case _ => PartialSecureBlock(Permissions.god)
  }
}
