package controllers

import java.util.UUID

import dao._
import models.Permissions.{god, reportCardEntry}
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
                                              val roleService: RoleServiceLike,
                                              val abstractDao: ReportCardEntryDao,
                                              val scheduleEntryDao: ScheduleEntryDao,
                                              val assignmentPlanService: AssignmentPlanService
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
    case Get => PartialSecureBlock(reportCardEntry.get)
    case _ => PartialSecureBlock(god)
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case Create => SecureBlock(restrictionId, reportCardEntry.create)
    case Update => SecureBlock(restrictionId, reportCardEntry.update)
    case GetAll => SecureBlock(restrictionId, reportCardEntry.getAll)
    case _ => PartialSecureBlock(god)
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

  def createByCopy(course: String) = restrictedContext(course)(Create) asyncContentTypedAction { request =>
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
