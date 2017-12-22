package controllers

import java.util.UUID

import dao._
import models._
import models.Role.{CourseEmployee, CourseManager}
import play.api.libs.json.{Reads, Writes}
import services.SessionHandlingService
import store.{ReportCardRetryTable, TableFilter}
import utils.LwmMimeType
import utils.LwmDateTime._

import scala.util.{Failure, Try}

object ReportCardRetryController {
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

final class ReportCardRetryController(val sessionService: SessionHandlingService,
                                      val authorityDao: AuthorityDao,
                                      val abstractDao: ReportCardRetryDao)
  extends AbstractCRUDControllerPostgres[PostgresReportCardRetryProtocol, ReportCardRetryTable, ReportCardRetryDb, ReportCardRetry] {

  import controllers.ReportCardRetryController._

  override implicit val mimeType: LwmMimeType = LwmMimeType.reportCardRetryV1Json

  override protected implicit val writes: Writes[ReportCardRetry] = ReportCardRetry.writes

  override protected implicit val reads: Reads[PostgresReportCardRetryProtocol] = PostgresReportCardRetryProtocol.reads

  override protected def tableFilter(attribute: String, value: String)(appendTo: Try[List[TableFilter[ReportCardRetryTable]]]): Try[List[TableFilter[ReportCardRetryTable]]] = {
    (appendTo, (attribute, value)) match { // TODO more attributes
      case (list, (`reportCardEntryAttribute`, reportCardEntry)) => list.map(_.+:(ReportCardRetryEntryFilter(reportCardEntry)))
      case (list, (`labworkAttribute`, labwork)) => list.map(_.+:(ReportCardRetryLabworkFilter(labwork)))
      case (list, (`courseAttribute`, course)) => list.map(_.+:(ReportCardRetryCourseFilter(course)))
      case _ => Failure(new Throwable("Unknown attribute"))
    }
  }

  override protected def toDbModel(protocol: PostgresReportCardRetryProtocol, existingId: Option[UUID]): ReportCardRetryDb = {
    val uuid = existingId getOrElse UUID.randomUUID
    val entryTypes = protocol.entryTypes.map(t => ReportCardEntryTypeDb(None, Some(uuid), t.entryType, t.bool, t.int))

    ReportCardRetryDb(protocol.reportCardEntry, protocol.date.sqlDate, protocol.start.sqlTime, protocol.end.sqlTime, protocol.room, entryTypes, protocol.reason, id = uuid)
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