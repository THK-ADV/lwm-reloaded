package controllers

import java.util.UUID

import dao._
import models.Permissions.{god, reportCardEntry}
import models.Role._
import models._
import play.api.libs.json.{Reads, Writes}
import services._
import store.{ReportCardEntryTable, TableFilter}
import utils.LwmMimeType

import scala.util.{Failure, Try}

object ReportCardEntryControllerPostgres {
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

final class ReportCardEntryControllerPostgres(val sessionService: SessionHandlingService,
                                              val authorityDao: AuthorityDao,
                                              val abstractDao: ReportCardEntryDao,
                                              val scheduleEntryDao: ScheduleEntryDao,
                                              val assignmentPlanService: AssignmentPlanDao
                                             ) extends AbstractCRUDControllerPostgres[PostgresReportCardEntryProtocol, ReportCardEntryTable, ReportCardEntryDb, ReportCardEntry] {
  import controllers.ReportCardEntryControllerPostgres._

  import scala.concurrent.ExecutionContext.Implicits.global

  override protected implicit val writes: Writes[ReportCardEntry] = ReportCardEntry.writes

  override protected implicit val reads: Reads[PostgresReportCardEntryProtocol] = PostgresReportCardEntry.reads

  override protected def tableFilter(attribute: String, value: String)(appendTo: Try[List[TableFilter[ReportCardEntryTable]]]): Try[List[TableFilter[ReportCardEntryTable]]] = {
    (appendTo, (attribute, value)) match { // TODO CONTINUE
      case (list, (`studentAttribute`, student)) => list.map(_.+:(ReportCardEntryStudentFilter(student)))
      case (list, (`courseAttribute`, course)) => list.map(_.+:(ReportCardEntryCourseFilter(course)))
      case (list, (`labworkAttribute`, labwork)) => list.map(_.+:(ReportCardEntryLabworkFilter(labwork)))
      case (list, (`roomAttribute`, room)) => list.map(_.+:(ReportCardEntryRoomFilter(room)))
      case (list, (`scheduleEntryAttribute`, sEntry)) => list.map(_.+:(ReportCardEntryScheduleEntryFilter(sEntry)))
      case _ => Failure(new Throwable("Unknown attribute"))
    }
  }

  override protected def toDbModel(protocol: PostgresReportCardEntryProtocol, existingId: Option[UUID]): ReportCardEntryDb = ???

  override implicit val mimeType = LwmMimeType.reportCardEntryV1Json

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(List(Student, CourseAssistant))
    case _ => PartialSecureBlock(List(God))
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case Create => SecureBlock(restrictionId, List(CourseManager))
    case Update => SecureBlock(restrictionId, List(CourseManager, CourseEmployee))
    case GetAll => SecureBlock(restrictionId, List(CourseManager, CourseEmployee, CourseAssistant))
    case _ => PartialSecureBlock(List(God))
  }

  def getForStudent(student: String) = contextFrom(Get) asyncAction { request =>
    all(NonSecureBlock)(request.append(studentAttribute -> Seq(student)))
  }

  def allFromScheduleEntry(course: String, scheduleEntry: String) = restrictedContext(course)(GetAll) asyncAction { request =>
    all(NonSecureBlock)(request.append(scheduleEntryAttribute -> Seq(course))) // TODO don't know if we can make something like this
  }

  def allFrom(course: String) = restrictedContext(course)(GetAll) asyncAction { request =>
    all(NonSecureBlock)(request.append(courseAttribute -> Seq(course)))
  }

  def createFrom(course: String, labwork: String) = restrictedContext(course)(Create) asyncContentTypedAction { request =>
    (for {
      schedules <- scheduleEntryDao.scheduleGenBy(labwork) if schedules.size == 1
      plans <- assignmentPlanService.get(List(AssignmentPlanLabworkFilter(labwork)), atomic = false) if plans.size == 1
      plan = plans.head.asInstanceOf[PostgresAssignmentPlan]
      reportCardEntries = ReportCardService.reportCards(schedules.head, plan)
      _ <- abstractDao.createMany(reportCardEntries.toList)
    } yield reportCardEntries.map(_.toLwmModel)).jsonResult
  }
}
