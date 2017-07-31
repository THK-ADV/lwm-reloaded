package controllers

import java.util.UUID

import dao._
import models.User.UserProtocol
import models._
import play.api.libs.json.{Json, Reads, Writes}
import services._
import store.{TableFilter, UserTable}
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

final class UserControllerPostgres(val roleService: RoleServiceLike, val sessionService: SessionHandlingService, val ldapService: LdapService, val abstractDao: UserDao)
  extends AbstractCRUDControllerPostgres[UserProtocol, UserTable, DbUser, User] {

  import scala.concurrent.ExecutionContext.Implicits.global

  override implicit val mimeType = LwmMimeType.userV1Json

  override def create(secureContext: SecureContext) = secureContext asyncContentTypedAction { request =>
    import models.PostgresAuthority.writesAtom
    import models.User.writes

    (for {
      userProtocol <- Future.fromTry(parse[UserProtocol](request))
      ldapUser <- ldapService.user2(userProtocol.systemId)
      userWithAuth <- abstractDao.createOrUpdate(ldapUser)
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

    abstractDao.buddyResult(requesterId, systemId, labwork).jsonResult { buddyResult =>
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
        case NotExisting => BadRequest(Json.obj(
          "status" -> "KO",
          "type" -> buddyResult.toString,
          "message" -> s"Dein Partner $systemId hat sich noch nicht im Praktikumstool angemeldet."
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

  override protected implicit val writes: Writes[User] = User.writes

  override protected implicit val reads: Reads[UserProtocol] = User.reads

  override protected def tableFilter(attribute: String, value: String)(appendTo: Try[List[TableFilter[UserTable]]]): Try[List[TableFilter[UserTable]]] = {
    import controllers.UserControllerPostgres._

    (appendTo, (attribute, value)) match {
      case (list, (`statusAttribute`, status)) => list.map(_.+:(UserStatusFilter(status)))
      case (list, (`systemIdAttribute`, systemId)) => list.map(_.+:(UserSystemIdFilter(systemId)))
      case (list, (`lastnameAttribute`, lastname)) => list.map(_.+:(UserLastnameFilter(lastname)))
      case (list, (`firstnameAttribute`, firstname)) => list.map(_.+:(UserFirstnameFilter(firstname)))
      case (list, (`degreeAttribute`, degree)) => list.map(_.+:(UserDegreeFilter(degree)))
      case _ => Failure(new Throwable("Unknown attribute"))
    }
  }

  override protected def toDbModel(protocol: UserProtocol, existingId: Option[UUID]): DbUser = ???
}
