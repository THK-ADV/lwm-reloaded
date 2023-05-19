package controllers

import controllers.helper.RequestOps
import dao._
import database.helper.LdapUserStatus
import database.{UserDb, UserTable}
import models.UserProtocol.{EmployeeProtocol, StudentProtocol}
import models._
import models.helper.{Allowed, Almost, Denied, NotExisting}
import play.api.libs.json.{JsNull, Json, Reads, Writes}
import play.api.mvc.ControllerComponents
import security.LWMRole._
import security.SecurityActionChain

import java.util.UUID
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

object UserController {
  lazy val statusAttribute = "status"
  lazy val degreeAttribute = "degree"
  lazy val systemIdAttribute = "systemId"
  lazy val firstnameAttribute = "firstname"
  lazy val lastnameAttribute = "lastname"
}

@Singleton
final class UserController @Inject() (
    cc: ControllerComponents,
    val authorityDao: AuthorityDao,
    val abstractDao: UserDao,
    val securedAction: SecurityActionChain,
    implicit val ctx: ExecutionContext
) extends AbstractCRUDController[UserProtocol, UserTable, UserDb, User](cc) {

  import UserController._
  import dao.UserDao._
  import utils.Ops._

  override protected implicit val writes: Writes[User] = User.writes

  override protected implicit val reads: Reads[UserProtocol] =
    UserProtocol.reads

  override protected def makeTableFilter(
      attribute: String,
      value: String
  ): Try[TableFilterPredicate] =
    (attribute, value) match {
      case (`statusAttribute`, s)    => LdapUserStatus(s) map statusFilter
      case (`degreeAttribute`, d)    => d.uuid map enrollmentFilter
      case (`systemIdAttribute`, s)  => Success(systemIdFilter(s))
      case (`firstnameAttribute`, f) => Success(firstnameFilter(f))
      case (`lastnameAttribute`, l)  => Success(lastnameFilter(l))
      case _                         => Failure(new Throwable(s"Unknown attribute $attribute"))
    }

  override protected def toDbModel(
      protocol: UserProtocol,
      existingId: Option[UUID]
  ): UserDb = {
    val id = existingId.getOrElse(UUID.randomUUID)

    protocol match {
      case StudentProtocol(
            systemId,
            campusId,
            lastname,
            firstname,
            email,
            registrationId,
            enrollment
          ) =>
        UserDb(
          systemId,
          campusId,
          lastname,
          firstname,
          email,
          LdapUserStatus.StudentStatus,
          Some(registrationId),
          Some(enrollment),
          id = id
        )
      case EmployeeProtocol(systemId, campusId, lastname, firstname, email) =>
        UserDb(
          systemId,
          campusId,
          lastname,
          firstname,
          email,
          LdapUserStatus.EmployeeStatus,
          None,
          None,
          id = id
        )
    }
  }

  def createFromToken() = securedAction.authorizationAction.async { request =>
    val result = request.unwrapped.userToken match {
      case Some(token) =>
        for {
          result <- abstractDao.createOrUpdateWithBasicAuthority(
            token.systemId,
            token.campusId,
            token.lastName,
            token.firstName,
            token.email,
            token.status,
            token.registrationId,
            token.degreeAbbrev
          )
        } yield result.entity.toUniqueEntity
      case None =>
        Future.failed(
          new Throwable(s"no ${RequestOps.UserToken} found in request")
        )
    }

    result.created
  }

  def buddy(labwork: String, student: String, buddy: String) =
    contextFrom(Get) asyncAction { _ =>
      val buddyResult = for {
        labworkId <- labwork.uuidF
        studentId <- student.uuidF
        result <- abstractDao.buddyResult(studentId, buddy, labworkId)
      } yield result

      buddyResult.jsonResult {
        case Allowed(b) =>
          Ok(
            Json.obj(
              "status" -> "OK",
              "type" -> buddyResult.toString,
              "buddy" -> Json.toJson(b),
              "message" -> s"Dein Partner ${b.systemId} hat Dich ebenfalls referenziert."
            )
          )
        case Almost(b) =>
          Ok(
            Json.obj(
              "status" -> "OK",
              "type" -> buddyResult.toString,
              "buddy" -> Json.toJson(b),
              "message" -> s"Dein Partner ${b.systemId} muss Dich ebenfalls referenzieren, ansonsten wird dieser Partnerwunsch nicht berücksichtigt."
            )
          )
        case Denied(b) =>
          Ok(
            Json.obj(
              "status" -> "KO",
              "type" -> buddyResult.toString,
              "buddy" -> Json.toJson(b),
              "message" -> s"Dein Partner ${b.systemId} und Du sind nicht im selben Studiengang."
            )
          )
        case NotExisting(b) =>
          Ok(
            Json.obj(
              "status" -> "KO",
              "type" -> buddyResult.toString,
              "buddy" -> JsNull,
              "message" -> s"Dein Partner $b existiert nicht oder hat sich noch nicht im Praktikumstool angemeldet."
            )
          )
      }
    }

  def getByCampusIds() = contextFrom(Update) asyncAction { request =>
    val map = valueOf(request.queryString)("map")

    for {
      text <- request.body.asText.toFuture
      campusIds = text.split("\n").foldLeft(List.empty[String]) {
        case (xs, x) => x.toLowerCase :: xs
      }
      users <- abstractDao.getByCampusIds(campusIds)
    } yield {
      val (found, notFound) =
        campusIds.foldLeft((List.empty[User], List.empty[String])) {
          case ((found, notFound), cid) =>
            users.find(_.campusId == cid) match {
              case Some(user) => (user :: found, notFound)
              case None       => (found, cid :: notFound)
            }
        }
      val foundJs =
        if (map.contains("systemId")) Json.toJson(found.map(_.systemId))
        else Json.toJson(found)

      ok(
        "found" -> foundJs,
        "notFound" -> notFound
      )
    }
  }

  def resolveStudentsByName() = contextFrom(Update) asyncAction { request =>
    for {
      text <- request.body.asText.toFuture
      students = text.split("\n").map { name =>
        val names = name.split(",")
        (names.head.trim, names.last.trim)
      }
      users <- abstractDao.get(
        List(statusFilter(LdapUserStatus.StudentStatus)),
        atomic = false
      )
      (gmids, duplicates, unknown) = students.foldLeft(
        (List.empty[String], List.empty[User], List.empty[String])
      ) { case ((found, duplicates, unknown), (lastname, firstname)) =>
        users
          .filter(s => s.lastname == lastname && s.firstname == firstname)
          .toList match {
          case h :: Nil =>
            (h.systemId :: found, duplicates, unknown)
          case h :: t =>
            (found, (h :: duplicates) ::: t, unknown)
          case Nil =>
            (found, duplicates, s"$lastname,$firstname" :: unknown)
        }
      }
    } yield ok(
      "gmids" -> gmids.mkString(","),
      "duplicates" -> Json.toJson(duplicates),
      "unknown" -> Json.toJson(unknown)
    )
  }

  def allFrom(course: String) = restrictedContext(course)(GetAll) asyncAction {
    request =>
      super.all(NonSecureBlock)(request)
  }

  def allStudentsRestricted(course: String) =
    restrictedContext(course)(Get) asyncAction { request =>
      (for {
        id <- Future.fromTry(
          request.systemId.toTry("No User ID found in request")
        )
        auths <- authorityDao.authoritiesFor(id)
        courses = auths.filter(_.course.isDefined).map(_.course.get)
        users <- abstractDao.get(
          List(
            UserDao.statusFilter(LdapUserStatus.StudentStatus),
            UserDao.applicationInCourseFilter(courses)
          ),
          atomic = false
        )
      } yield users).jsonResult
    }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get    => PartialSecureBlock(List(Admin))
    case GetAll => PartialSecureBlock(List(Admin))
    case Update => PartialSecureBlock(List(Admin))
    case _      => PartialSecureBlock(List(God))
  }

  override protected def restrictedContext(
      restrictionId: String
  ): PartialFunction[Rule, SecureContext] = {
    case Get =>
      SecureBlock(
        restrictionId,
        List(CourseAssistant, CourseEmployee, CourseManager)
      )
    case GetAll => SecureBlock(restrictionId, List(CourseManager))
  }
}
