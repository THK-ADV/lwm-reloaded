package controllers

import java.util.UUID

import dao._
import database.{ReportCardEntryDb, ReportCardEntryTable}
import javax.inject.{Inject, Singleton}
import models.Role._
import models._
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.ControllerComponents
import security.SecurityActionChain
import services._

import scala.concurrent.Future

object ReportCardEntryController {
  lazy val studentAttribute = "student"
  lazy val courseAttribute = "course"
  lazy val labworkAttribute = "labwork"
  lazy val roomAttribute = "room"
  lazy val scheduleEntryAttribute = "scheduleEntry"

  lazy val dateAttribute = "date"
  lazy val startAttribute = "start"
  lazy val endAttribute = "end"
  lazy val sinceAttribute = "since"
  lazy val untilAttribute = "until"
}

@Singleton
final class ReportCardEntryController @Inject()(cc: ControllerComponents, val authorityDao: AuthorityDao, val abstractDao: ReportCardEntryDao, val scheduleEntryDao: ScheduleEntryDao, val assignmentPlanService: AssignmentPlanDao, val securedAction: SecurityActionChain)
  extends AbstractCRUDController[ReportCardEntryProtocol, ReportCardEntryTable, ReportCardEntryDb, ReportCardEntryLike](cc) {

  import controllers.ReportCardEntryController._

  import scala.concurrent.ExecutionContext.Implicits.global

  override protected implicit val writes: Writes[ReportCardEntryLike] = ReportCardEntryLike.writes

  override protected implicit val reads: Reads[ReportCardEntryProtocol] = ReportCardEntryProtocol.reads

//  override protected def tableFilter(attribute: String, value: String)(appendTo: Try[List[TableFilter[ReportCardEntryTable]]]): Try[List[TableFilter[ReportCardEntryTable]]] = {
//    (appendTo, (attribute, value)) match {
//      case (list, (`studentAttribute`, student)) => list.map(_.+:(ReportCardEntryStudentFilter(student)))
//      case (list, (`courseAttribute`, course)) => list.map(_.+:(ReportCardEntryCourseFilter(course)))
//      case (list, (`labworkAttribute`, labwork)) => list.map(_.+:(ReportCardEntryLabworkFilter(labwork)))
//      case (list, (`roomAttribute`, room)) => list.map(_.+:(ReportCardEntryRoomFilter(room)))
//      case (list, (`scheduleEntryAttribute`, sEntry)) => list.map(_.+:(ReportCardEntryScheduleEntryFilter(sEntry)))
//      case (list, (`dateAttribute`, date)) => list.map(_.+:(ReportCardEntryDateFilter(date)))
//      case (list, (`startAttribute`, start)) => list.map(_.+:(ReportCardEntryStartFilter(start)))
//      case (list, (`endAttribute`, end)) => list.map(_.+:(ReportCardEntryEndFilter(end)))
//      case (list, (`sinceAttribute`, since)) => list.map(_.+:(ReportCardEntrySinceFilter(since)))
//      case (list, (`untilAttribute`, until)) => list.map(_.+:(ReportCardEntryUntilFilter(until)))
//      case _ => Failure(new Throwable("Unknown attribute"))
//    }
//  }

  override protected def toDbModel(protocol: ReportCardEntryProtocol, existingId: Option[UUID]): ReportCardEntryDb = ???

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(List(StudentRole, CourseAssistant))
    case _ => PartialSecureBlock(List(God))
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case Create => SecureBlock(restrictionId, List(CourseManager))
    case Update => SecureBlock(restrictionId, List(CourseManager, CourseEmployee))
    case GetAll => SecureBlock(restrictionId, List(CourseManager, CourseEmployee, CourseAssistant))
    case _ => PartialSecureBlock(List(God))
  }

  def getForStudent(student: String) = contextFrom(Get) asyncAction { request =>
    all(NonSecureBlock)(request.appending(studentAttribute -> Seq(student)))
  }

  def allFromScheduleEntry(course: String, scheduleEntry: String) = restrictedContext(course)(GetAll) asyncAction { request =>
    all(NonSecureBlock)(request.appending(scheduleEntryAttribute -> Seq(scheduleEntry)))
  }

  def allFrom(course: String) = restrictedContext(course)(GetAll) asyncAction { request =>
    all(NonSecureBlock)(request.appending(courseAttribute -> Seq(course)))
  }

  def createFrom(course: String, labwork: String) = restrictedContext(course)(Create) asyncAction { _ =>
    (for {
      labworkId <- Future.fromTry(labwork.uuid)
      schedules <- scheduleEntryDao.scheduleGenBy(labwork) if schedules.isDefined
      maybePlan <- assignmentPlanService.getSingleWhere(AssignmentPlanDao.labworkFilter(labworkId).apply, atomic = false) if maybePlan.isDefined
      reportCardEntries = ReportCardService.reportCards(schedules.head, maybePlan.head)
      _ <- abstractDao.createMany(reportCardEntries.toList)
    } yield reportCardEntries.map(_.toUniqueEntity)).jsonResult
  }

  def createByCopy(course: String) = restrictedContext(course)(Create) asyncAction { _ =>
    ???
  }
}

/** TODO
  * controller mit expander funktionen, beispielsweise
  * removeStudentFromLabwork -> delete group membership, lapp and reportCardEntries
  * insert student into group -> create lapp if needed, group membership with reportCardEntries
  * swapGroup
  * patch assignmentplan/timetable/group -> expand dependencies (reportcardEntries...)
  *
  */
