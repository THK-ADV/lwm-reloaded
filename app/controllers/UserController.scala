package controllers

import java.util.UUID

import controllers.core.AbstractCRUDController
import dao._
import database.helper.LdapUserStatus
import database.{UserDb, UserTable}
import javax.inject.{Inject, Singleton}
import models.Role._
import models._
import models.helper.{Allowed, Almost, Denied, NotExisting}
import play.api.libs.json.{Json, Reads, Writes}
import play.api.mvc.ControllerComponents
import security.SecurityActionChain

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success, Try}

object UserController {
  lazy val statusAttribute = "status"
  lazy val degreeAttribute = "degree"
  lazy val systemIdAttribute = "systemId"
  lazy val firstnameAttribute = "firstname"
  lazy val lastnameAttribute = "lastname"
}

@Singleton
final class UserController @Inject()(
  cc: ControllerComponents,
  val authorityDao: AuthorityDao,
  val abstractDao: UserDao,
  val securedAction: SecurityActionChain,
  implicit val ctx: ExecutionContext
) extends AbstractCRUDController[UserProtocol, UserTable, UserDb, User](cc) {

  import controllers.core.DBFilterOps._

  override protected implicit val writes: Writes[User] = User.writes

  override protected implicit val reads: Reads[UserProtocol] = UserProtocol.reads

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(List(StudentRole, EmployeeRole, CourseAssistant))
    case GetAll => PartialSecureBlock(List(StudentRole, EmployeeRole, CourseAssistant))
    case Create => PartialSecureBlock(List(Admin))
    case _ => PartialSecureBlock(List(God))
  }

  override protected def makeTableFilter(attribute: String, value: String): Try[TableFilterPredicate] = {
    import UserController._
    import dao.UserDao._

    (attribute, value) match {
      case (`statusAttribute`, s) => LdapUserStatus(s) map statusFilter
      case (`degreeAttribute`, d) => d.uuid map enrollmentFilter
      case (`systemIdAttribute`, s) => Success(systemIdFilter(s))
      case (`firstnameAttribute`, f) => Success(firstnameFilter(f))
      case (`lastnameAttribute`, l) => Success(lastnameFilter(l))
      case _ => Failure(new Throwable(s"Unknown attribute $attribute"))
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
    val buddyResult = for {
      labworkId <- labwork.uuidF
      studentId <- student.uuidF
      result <- abstractDao.buddyResult(studentId, buddy, labworkId)
    } yield result

    buddyResult.jsonResult {
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

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = forbiddenAction()
}
