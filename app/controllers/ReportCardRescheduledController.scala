package controllers

import java.util.UUID

import dao._
import models.{PostgresReportCardRescheduled, PostgresReportCardRescheduledProtocol, ReportCardRescheduled, ReportCardRescheduledDb}
import play.api.libs.json.{Reads, Writes}
import services.SessionHandlingService
import store.{ReportCardRescheduledTable, TableFilter}
import utils.LwmMimeType
import utils.LwmDateTime._
import models.Role.{CourseEmployee, CourseManager, God}

import scala.util.{Failure, Try}

object ReportCardRescheduledController {
  lazy val reportCardEntryAttribute = "reportCardEntry"

  lazy val studentAttribute = "student"
  lazy val labworkAttribute = "labwork"
  lazy val courseAttribute = "course"
  lazy val roomAttribute = "room"

  lazy val dateAttribute = "date"
  lazy val startAttribute = "start"
  lazy val endAttribute = "end"
  lazy val sinceAttribute = "since"
  lazy val untilAttribute = "until"
}

final class ReportCardRescheduledController(val sessionService: SessionHandlingService,
                                            val authorityDao: AuthorityDao,
                                            val abstractDao: ReportCardRescheduledDao)
  extends AbstractCRUDControllerPostgres[PostgresReportCardRescheduledProtocol, ReportCardRescheduledTable, ReportCardRescheduledDb, ReportCardRescheduled] {

  import controllers.ReportCardRescheduledController._

  override implicit val mimeType: LwmMimeType = LwmMimeType.reportCardRescheduledV1Json

  override protected implicit val writes: Writes[ReportCardRescheduled] = ReportCardRescheduled.writes

  override protected implicit val reads: Reads[PostgresReportCardRescheduledProtocol] = PostgresReportCardRescheduledProtocol.reads

  override protected def tableFilter(attribute: String, value: String)(appendTo: Try[List[TableFilter[ReportCardRescheduledTable]]]): Try[List[TableFilter[ReportCardRescheduledTable]]] = {
    (appendTo, (attribute, value)) match { // TODO more attributes
      case (list, (`reportCardEntryAttribute`, reportCardEntry)) => list.map(_.+:(ReportCardRescheduledEntryFilter(reportCardEntry)))
      case (list, (`labworkAttribute`, labwork)) => list.map(_.+:(ReportCardRescheduledLabworkFilter(labwork)))
      case (list, (`courseAttribute`, course)) => list.map(_.+:(ReportCardRescheduledCourseFilter(course)))
      case _ => Failure(new Throwable("Unknown attribute"))
    }
  }

  override protected def toDbModel(protocol: PostgresReportCardRescheduledProtocol, existingId: Option[UUID]): ReportCardRescheduledDb = {
    ReportCardRescheduledDb(protocol.reportCardEntry, protocol.date.sqlDate, protocol.start.sqlTime, protocol.end.sqlTime, protocol.room, protocol.reason, id = existingId getOrElse UUID.randomUUID)
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = { // TODO discuss
    case Create => SecureBlock(restrictionId, List(CourseManager))
    case Delete => SecureBlock(restrictionId, List(CourseManager))
    case GetAll => SecureBlock(restrictionId, List(CourseManager, CourseEmployee))
    case Get => SecureBlock(restrictionId, List(CourseManager, CourseEmployee))
    case Update => SecureBlock(restrictionId, List(CourseManager))
  }

  def createFrom(course: String) = restrictedContext(course)(Create) asyncContentTypedAction { request =>
    create(NonSecureBlock)(request)
  }

  def allFrom(course: String) = restrictedContext(course)(GetAll) asyncAction { request =>
    all(NonSecureBlock)(request.append(courseAttribute -> Seq(course)))
  }

  def getFrom(course: String, id: String) = restrictedContext(course)(Get) asyncAction { request =>
    get(id, NonSecureBlock)(request)
  }

  def updateFrom(course: String, id: String) = restrictedContext(course)(Update) asyncContentTypedAction { request =>
    update(id, NonSecureBlock)(request)
  }

  def deleteFrom(course: String, id: String) = restrictedContext(course)(Delete) asyncAction { request =>
    delete(id, NonSecureBlock)(request)
  }
}
