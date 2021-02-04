package controllers.serviceResource

import controllers.helper.{JsonParser, ResultOps, SecureControllerContext, Secured}
import dao.{AuthorityDao, LwmServiceDao}
import database.GroupMembership
import play.api.libs.json.{Json, OWrites}
import play.api.mvc.{AbstractController, ControllerComponents, Result}
import security.LWMRole.{Admin, CourseEmployee, CourseManager, God}
import security.SecurityActionChain

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

final class LwmServiceController @Inject()(
  cc: ControllerComponents,
  val authorityDao: AuthorityDao,
  val securedAction: SecurityActionChain,
  val serviceDao: LwmServiceDao,
  implicit val executionContext: ExecutionContext
) extends AbstractController(cc)
  with Secured
  with SecureControllerContext
  with ResultOps
  with JsonParser {

  private implicit def membershipWrites: OWrites[GroupMembership] = Json.writes[GroupMembership]

  import models.LabworkApplication.{writes => lappWrites}
  import models.ReportCardEntry.{writes => cardWrites}
  import models.Student.{writes => studentWrites}

  def insertStudentToGroup(course: String) = restrictedContext(course)(Create) asyncAction { request =>
    (parseJson[GroupChangeRequest] _ andThen mapJson andThen (r => r(insertIntoGroup))) (request)
  }

  def removeStudentFromGroup(course: String) = restrictedContext(course)(Delete) asyncAction { request =>
    (parseJson[GroupChangeRequest] _ andThen mapJson andThen (r => r(removeFromGroup))) (request)
  }

  def moveStudentToGroup(course: String) = restrictedContext(course)(Update) asyncAction { request =>
    (parseJson[GroupMovingRequest] _ andThen mapJson andThen (r => r(moveToGroup))) (request)
  }

  def mergeUsers() = contextFrom(Update) asyncAction { request =>
    (for {
      json <- Future.fromTry(unwrap(request))
      origin <- Future.fromTry(asTry(string(json.\("origin"))))
      drop <- Future.fromTry(asTry(string(json.\("drop"))))
      res <- serviceDao.mergeUser(origin, drop)
    } yield res).jsonResult
  }

  def duplicateUsers() = contextFrom(Get) asyncAction { _ =>
    serviceDao.duplicateStudents().jsonResult
  }

  def usersWithoutRegistrationId() = contextFrom(Get) asyncAction { _ =>
    serviceDao.usersWithoutRegistrationId().jsonResult
  }

  private def insertIntoGroup(request: GroupChangeRequest): Future[Result] = {
    serviceDao.insertStudentToGroup(request.student, request.labwork, request.group).map {
      case (membership, app, cards, _) => ok(
        "labworkApplication" -> Json.toJson(app),
        "membership" -> Json.toJson(membership),
        "reportCardEntries" -> Json.toJson(cards)
      )
    }
  }

  private def removeFromGroup(request: GroupChangeRequest): Future[Result] = {
    serviceDao.removeStudentFromGroup(request.student, request.labwork, request.group).map {
      case (groupDeleted, deleteApp, deletedCards) => ok(
        "changedMembership" -> Json.toJson(groupDeleted),
        "labworkApplication" -> Json.toJson(deleteApp),
        "reportCardEntries" -> Json.toJson(deletedCards)
      )
    }
  }

  private def moveToGroup(request: GroupMovingRequest): Future[Result] = {
    serviceDao.moveStudentToGroup(request.student, request.labwork, request.srcGroup, request.destGroup).map {
      case (groupDeleted, newMembership, _, _, updatedCards) => ok(
        "changedMembership" -> Json.toJson(groupDeleted),
        "newMembership" -> Json.toJson(newMembership),
        "updatedReportCardEntries" -> Json.toJson(updatedCards)
      )
    }
  }

  private def mapJson[A](json: Try[A])(f: A => Future[Result]): Future[Result] =
    json match {
      case Success(r) =>
        f(r)
      case Failure(e) =>
        (badRequest _ andThen Future.successful) (e)
    }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case Update => SecureBlock(restrictionId, List(CourseEmployee, CourseManager))
    case Create | Delete => SecureBlock(restrictionId, List(CourseManager))
    case _ => PartialSecureBlock(List(God))
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Update => PartialSecureBlock(List(Admin))
    case Get => PartialSecureBlock(List(Admin))
  }
}
