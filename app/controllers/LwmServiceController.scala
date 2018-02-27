package controllers

import java.util.UUID

import controllers.helper._
import dao._
import models.Role.{Admin, CourseManager}
import models._
import play.api.libs.json.{Json, Writes}
import play.api.mvc.Controller
import services.SessionHandlingService
import utils.LwmMimeType

final class LwmServiceController(val authorityDao: AuthorityDao,
                                 val sessionService: SessionHandlingService,
                                 val lwmServiceDao: LwmServiceDao)
  extends Controller
    with Secured
    with SessionChecking
    with SecureControllerContext
    with ContentTyped
    with PostgresResult
    with AttributeFilter
    with RequestRebase {

  override implicit val mimeType: LwmMimeType = LwmMimeType.lwmServiceV1Json

  private def toJson(membership: GroupMembership) = Json.obj("student" -> membership.student, "group" -> membership.group)

  private def toJson(cards: Seq[ReportCardEntryDb]) = {
    implicit val seqWrites: Writes[Seq[PostgresReportCardEntry]] = Writes.seq[PostgresReportCardEntry](PostgresReportCardEntry.writes)

    Json.toJson(cards.map(_.toLwmModel))
  }

  def removeStudentFromLabwork(course: String, labwork: String, group: String, student: String) = ??? // TODO remove lapp, group membership and reportCards

  // create lapp, add to group, copy reportCardEntries from first member in group
  def addStudentToLabwork(course: String, labwork: String, group: String, student: String) = restrictedContext(course)(Create) asyncContentTypedAction { _ =>
    lwmServiceDao.addStudentToLabwork(UUID.fromString(student), UUID.fromString(labwork), UUID.fromString(group)).jsonResult { res =>
      val (lapp, membership, cards) = res

      Created(Json.obj(
        "status" -> "OK",
        "labworkApplication" -> Json.toJson(lapp.toLwmModel)(PostgresLabworkApplication.writes),
        "group membership" -> toJson(membership),
        "reportCardEntries" -> toJson(cards)
      ))
    }
  }

  def appendReportCardEntries(course: String, labwork: String, preview: String) = ???

  def removeReportCardEntries(course: String, labwork: String, preview: String) = ???

  def moveStudentToGroup(course: String, labwork: String, group: String, student: String) = restrictedContext(course)(Update) asyncContentTypedAction { _ =>
    lwmServiceDao.moveStudentToGroup(UUID.fromString(student), UUID.fromString(labwork), UUID.fromString(group)).jsonResult { res =>
      val (membership, oldCards, updatedCards) = res

      Ok(Json.obj(
        "status" -> "OK",
        "group membership" -> toJson(membership),
        "old cards" -> toJson(oldCards),
        "new cards" -> toJson(updatedCards.flatten)
      ))
    }
  }

  def swapSupervisors(course: String, labwork: String, preview: String) = ??? // TODO strategy: entries since x

  def swapReportCardAssignments(course: String, labwork: String, preview: String) = ???

  def collisionsForCurrentLabworks(course: String) = ???

  def multipleReportCardEntries(course: String) = ???

  def multipleLabworkApplications(course: String) = restrictedContext(course)(GetAll) asyncAction { _ =>
    import models.LabworkApplication.writes

    lwmServiceDao.multipleLabworkApplications(course).jsonResult { res =>
      Ok(Json.obj(
        "status" -> "OK",
        "multiple labwork applications" -> res.map {
          case (student, lapps) => Json.obj(
            "student" -> student,
            "labwork applications" -> Json.toJson(lapps.map(_.toLwmModel))
          )
        }
      ))
    }
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = { // TODO TDB roles
    case Create => SecureBlock(restrictionId, List(CourseManager))
    case Update => SecureBlock(restrictionId, List(CourseManager))
    case GetAll => SecureBlock(restrictionId, List(CourseManager))
    case _ => PartialSecureBlock(List(Admin))
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = { // TODO TDB roles
    case _ => PartialSecureBlock(List(Admin))
  }
}
