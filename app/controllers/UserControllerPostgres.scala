package controllers

import models._
import play.api.libs.json.Json
import play.api.mvc.Controller
import services._
import store.{Resolvers, TableFilter, UserTable}
import utils.LwmMimeType

import scala.concurrent.Future
import scala.util.{Failure, Try}

object UserControllerPostgres {
  lazy val statusAttribute = "status"
  lazy val degreeAttribute = "degree"
  lazy val systemIdAttribute = "systemId"
  lazy val firstnameAttribute = "firstname"
  lazy val lastnameAttribute = "lastname"
}

final class UserControllerPostgres(val roleService: RoleService, val sessionService: SessionHandlingService, val resolvers: Resolvers, val ldapService: LdapService, val userService: UserService) extends Controller
  with Secured
  with SessionChecking
  with SecureControllerContext
  with ContentTyped
  with Chunked
  with PostgresResult {

  import scala.concurrent.ExecutionContext.Implicits.global

  override implicit def mimeType = LwmMimeType.userV1Json

  import models.User.{writes, reads, UserProtocol}

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

  def createOrUpdate() = contextFrom(Create) asyncContentTypedAction { request =>
    import models.User.writes
    import models.PostgresAuthority.writesAtom

    (for {
      userProtocol <- Future.fromTry(parse[UserProtocol](request))
      ldapUser <- ldapService.user2(userProtocol.systemId)
      userWithAuth <- userService.createOrUpdate(ldapUser)
    } yield userWithAuth).jsonResult { userWithAuth =>
      val (user, maybeAuth) = userWithAuth
      val userJson = Json.toJson(user)

      maybeAuth.fold {
        Ok(userJson)
      } { auth =>
        Created(Json.obj(
          "user" -> userJson,
          "authority" -> Json.toJson(auth)
        ))
      }
    }
  }

  def buddy(systemId: String, labwork: String) = contextFrom(Get) asyncAction { request =>
    val requesterId = request.session(SessionController.userId)

    userService.buddyResult(requesterId, systemId, labwork).jsonResult { buddyResult =>
      buddyResult match {
        case Allowed => Ok(Json.obj(
          "status" -> "OK",
          "type" -> buddyResult.toString,
          "message" -> s"Dein Partner $systemId hat Dich ebenfalls referenziert."
        ))
        case Almost => Ok(Json.obj(
          "status" -> "OK",
          "type" -> buddyResult.toString,
          "message" -> s"Dein Partner $systemId muss Dich ebenfalls referenzieren, ansonsten wird dieser Partnerwunsch nicht berücksichtigt."
        ))
        case Denied => BadRequest(Json.obj(
          "status" -> "KO",
          "type" -> buddyResult.toString,
          "message" -> s"Dein Partner $systemId und Du sind nicht im selben Studiengang."
        ))
      }
    }
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(Permissions.user.get)
    case GetAll => PartialSecureBlock(Permissions.user.getAll)
    case Create => PartialSecureBlock(Permissions.prime)
    case _ => PartialSecureBlock(Permissions.god)
  }
}
