package controllers

import java.util.UUID

import dao._
import javax.inject.{Inject, Singleton}
import models.Role._
import models._
import models.helper.{Allowed, Almost, Denied, NotExisting}
import play.api.libs.json.{Json, Reads, Writes}
import play.api.mvc.ControllerComponents
import store.{TableFilter, UserDb, UserTable}
import utils.SecuredAction

import scala.util.{Failure, Try}

object UserController {
  lazy val statusAttribute = "status"
  lazy val degreeAttribute = "degree"
  lazy val systemIdAttribute = "systemId"
  lazy val firstnameAttribute = "firstname"
  lazy val lastnameAttribute = "lastname"
}

@Singleton
final class UserController @Inject()(cc: ControllerComponents, val authorityDao: AuthorityDao, val abstractDao: UserDao, val securedAction: SecuredAction)
  extends AbstractCRUDController[UserProtocol, UserTable, UserDb, User](cc) {

  import scala.concurrent.ExecutionContext.Implicits.global

  override protected implicit val writes: Writes[User] = User.writes

  override protected implicit val reads: Reads[UserProtocol] = UserProtocol.reads

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(List(StudentRole, EmployeeRole, CourseAssistant))
    case GetAll => PartialSecureBlock(List(StudentRole, EmployeeRole, CourseAssistant))
    case Create => PartialSecureBlock(List(Admin))
    case _ => PartialSecureBlock(List(God))
  }

  override protected def tableFilter(attribute: String, value: String)(appendTo: Try[List[TableFilter[UserTable]]]): Try[List[TableFilter[UserTable]]] = {
    import controllers.UserController._

    (appendTo, (attribute, value)) match {
      case (list, (`statusAttribute`, status)) => list.map(_.+:(UserStatusFilter(status)))
      case (list, (`systemIdAttribute`, systemId)) => list.map(_.+:(UserSystemIdFilter(systemId)))
      case (list, (`lastnameAttribute`, lastname)) => list.map(_.+:(UserLastnameFilter(lastname)))
      case (list, (`firstnameAttribute`, firstname)) => list.map(_.+:(UserFirstnameFilter(firstname)))
      case (list, (`degreeAttribute`, degree)) => list.map(_.+:(UserDegreeFilter(degree)))
      case _ => Failure(new Throwable("Unknown attribute"))
    }
  }

  override protected def toDbModel(protocol: UserProtocol, existingId: Option[UUID]): UserDb = ???

  //  override def create(secureContext: SecureContext) = secureContext asyncContentTypedAction { request =>
  //    import models.User.writes
  //
  //    (for {
  //      userProtocol <- Future.fromTry(parse[UserProtocol](request))
  //      ldapUser <- ldapService.user(userProtocol.systemId)
  //      userWithAuth <- abstractDao.createOrUpdate(ldapUser)
  //    } yield userWithAuth).jsonResult { userWithAuth =>
  //      val (user, maybeAuth) = userWithAuth
  //      val userJson = Json.toJson(user)
  //
  //      maybeAuth.fold {
  //        Ok(userJson)
  //      } { auth =>
  //        Created(Json.obj(
  //          "user" -> userJson,
  //          "authority" -> Json.toJson(auth)(PostgresAuthorityAtom.writes)
  //        ))
  //      }
  //    }
  //  }

  def buddy(labwork: String, student: String, buddy: String) = contextFrom(Get) asyncAction { _ =>
    abstractDao.buddyResult(student, buddy, labwork).jsonResult { buddyResult =>
      buddyResult match {
        case Allowed(b) => Ok(Json.obj(
          "status" -> "OK",
          "type" -> buddyResult.toString,
          "buddy" -> Json.toJson(b),
          "message" -> s"Dein Partner ${b.systemId} hat Dich ebenfalls referenziert."
        ))
        case Almost(b) => Ok(Json.obj(
          "status" -> "OK",
          "type" -> buddyResult.toString,
          "buddy" -> Json.toJson(b),
          "message" -> s"Dein Partner ${b.systemId} muss Dich ebenfalls referenzieren, ansonsten wird dieser Partnerwunsch nicht berücksichtigt."
        ))
        case Denied(b) => Ok(Json.obj(
          "status" -> "KO",
          "type" -> buddyResult.toString,
          "buddy" -> Json.toJson(b),
          "message" -> s"Dein Partner ${b.systemId} und Du sind nicht im selben Studiengang."
        ))
        case NotExisting(b) => Ok(Json.obj(
          "status" -> "KO",
          "type" -> buddyResult.toString,
          "buddy" -> None,
          "message" -> s"Dein Partner $b existiert nicht oder hat sich noch nicht im Praktikumstool angemeldet."
        ))
      }
    }
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = forbidden()
}
