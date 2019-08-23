package controllers

import java.util.UUID

import controllers.helper.{JsonParser, ResultOps, SecureControllerContext, Secured}
import dao.{AuthorityDao, LwmServiceDao}
import database.GroupMembership
import javax.inject.Inject
import models.Role.{CourseEmployee, CourseManager, God}
import play.api.libs.json.{Json, OWrites}
import play.api.mvc.{AbstractController, ControllerComponents}
import security.SecurityActionChain

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

final class LwmServiceController @Inject()(
  cc: ControllerComponents,
  val authorityDao: AuthorityDao,
  val securedAction: SecurityActionChain,
  val serviceDao: LwmServiceDao,
  implicit val executionContext: ExecutionContext
)
  extends AbstractController(cc)
    with Secured
    with SecureControllerContext
    with ResultOps
    with JsonParser {

  private implicit def membershipWrites: OWrites[GroupMembership] = Json.writes[GroupMembership]

  def insertStudentToGroup(course: String, labwork: String, group: String, student: String) = restrictedContext(course)(Create) asyncAction { _ =>
    val ids = for {
      lid <- Try(UUID.fromString(labwork))
      gid <- Try(UUID.fromString(group))
      sid <- Try(UUID.fromString(student))
    } yield (lid, gid, sid)

    ids match {
      case Success((lid, gid, sid)) =>
        serviceDao.insertStudentToGroup(sid, lid, gid).map {
          case (app, membership, _, cards) => ok(
            "labworkApplication" -> Json.toJson(app.toUniqueEntity),
            "membership" -> Json.toJson(membership),
            "reportCards" -> Json.toJson(cards.map(_.toUniqueEntity))
          )
        }
      case Failure(e) =>
        (badRequest _ andThen Future.successful) (e)
    }
  }

  def moveStudentToGroup(course: String, labwork: String, srcGroup: String, student: String, destGroup: String) = restrictedContext(course)(Create) asyncAction { _ =>
    ???
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case Create => SecureBlock(restrictionId, List(CourseEmployee, CourseManager))
    case _ => PartialSecureBlock(List(God))
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = forbiddenAction()
}
