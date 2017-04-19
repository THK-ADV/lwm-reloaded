package controllers

import java.util.UUID

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

final class UserControllerPostgres(val roleService: RoleServiceLike, val sessionService: SessionHandlingService, val ldapService: LdapService, val userService: UserService)
  extends AbstractCRUDControllerPostgres[UserProtocol, UserTable, DbUser, User] {

  import scala.concurrent.ExecutionContext.Implicits.global

  override implicit val mimeType = LwmMimeType.userV1Json

  override def create = contextFrom(Create) asyncContentTypedAction { request =>
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

  override protected val abstractDao: AbstractDao[UserTable, DbUser, User] = userService

  override protected def idTableFilter(id: String): TableFilter[UserTable] = UserIdFilter(id)

  override protected def tableFilter(attribute: String, values: Seq[String])(appendTo: Try[List[TableFilter[UserTable]]]): Try[List[TableFilter[UserTable]]] = {
    import controllers.UserControllerPostgres._

    (appendTo, (attribute, values)) match {
      case (list, (`statusAttribute`, status)) => list.map(_.+:(UserStatusFilter(status.head)))
      case (list, (`systemIdAttribute`, systemId)) => list.map(_.+:(UserSystemIdFilter(systemId.head)))
      case (list, (`lastnameAttribute`, lastname)) => list.map(_.+:(UserLastnameFilter(lastname.head)))
      case (list, (`firstnameAttribute`, firstname)) => list.map(_.+:(UserFirstnameFilter(firstname.head)))
      case (list, (`degreeAttribute`, degree)) => list.map(_.+:(UserDegreeFilter(degree.head)))
      case _ => Failure(new Throwable("Unknown attribute"))
    }
  }

  override protected def toDbModel(protocol: UserProtocol, existingId: Option[UUID]): DbUser = ???

  override protected def toLwmModel(dbModel: DbUser): User = dbModel.toUser
}
