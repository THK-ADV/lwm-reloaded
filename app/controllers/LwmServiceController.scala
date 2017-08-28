package controllers

import java.util.UUID

import controllers.helper.{AttributeFilter, PostgresResult, SecureControllerContext2, Secured2}
import dao._
import models.Role.{Admin, CourseManager}
import models._
import play.api.libs.json.Json
import play.api.mvc.Controller
import services.SessionHandlingService
import utils.LwmMimeType

final class LwmServiceController(val authorityDao: AuthorityDao,
                                 val sessionService: SessionHandlingService,
                                 val lwmServiceDao: LwmServiceDao)
  extends Controller
    with Secured2
    with SessionChecking
    with SecureControllerContext2
    with ContentTyped
    with PostgresResult
    with AttributeFilter
    with RequestRebasePostgres {

  override implicit val mimeType: LwmMimeType = LwmMimeType.lwmServiceV1Json

  def removeStudentFromLabwork(course: String, labwork: String, student: String) = ??? // TODO remove lapp, group membership and reportCards

  def addStudentToLabwork(course: String, labwork: String, student: String, group: String) = restrictedContext(course)(Create) asyncContentTypedAction { _ =>
    lwmServiceDao.addStudentToLabwork(UUID.fromString(student), UUID.fromString(labwork), UUID.fromString(group)).jsonResult { res =>
      val (lapp, membership, cards) = res

      Created(Json.obj(
        "status" -> "OK",
        "labworkApplication" -> Json.toJson(lapp.toLwmModel)(PostgresLabworkApplication.writes),
        "groupMembership" -> Json.obj("student" -> membership.student, "group" -> membership.group),
        "reportCardEntries" -> Json.toJson(cards.map(_.toLwmModel))
      ))
    }
  }

  def appendReportCardEntries(course: String, labwork: String, preview: String) = ???

  def removeReportCardEntries(course: String, labwork: String, preview: String) = ???

  def swapStudentsInGroup(course: String, labwork: String) = ???

  def swapSupervisors(course: String, labwork: String, preview: String) = ???

  def swapReportCardAssignments(course: String, labwork: String, preview: String) = ???

  def collisionsForCurrentLabworks(course: String) = ???

  def multipleReportCardEntries(course: String) = ???

  def multipleLabworkApplications(course: String) = ???

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = { // TODO TDB roles
    case Create => SecureBlock(restrictionId, List(CourseManager))
    case _ => PartialSecureBlock(List(Admin))
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = { // TODO TDB roles
    case _ => PartialSecureBlock(List(Admin))
  }
}
