package controllers

import controllers.helper.TimeRangeTableFilter
import dao._
import database.{ReportCardEntryTypeDb, ReportCardRetryDb, ReportCardRetryTable}
import models._
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.ControllerComponents
import security.LWMRole.{CourseEmployee, CourseManager}
import security.SecurityActionChain
import utils.date.DateTimeOps._

import java.util.UUID
import javax.inject.{Inject, Singleton}
import scala.util.Try

object ReportCardRetryController {
  lazy val reportCardEntryAttribute = "reportCardEntry"
  lazy val studentAttribute = "student"
  lazy val labworkAttribute = "labwork"
  lazy val courseAttribute = "course"
  lazy val roomAttribute = "room"
}

@Singleton
final class ReportCardRetryController @Inject()(
  cc: ControllerComponents,
  val authorityDao: AuthorityDao,
  val abstractDao: ReportCardRetryDao,
  val securedAction: SecurityActionChain
) extends AbstractCRUDController[ReportCardRetryProtocol, ReportCardRetryTable, ReportCardRetryDb, ReportCardRetryLike](cc)
  with TimeRangeTableFilter[ReportCardRetryTable] {

  import controllers.ReportCardRetryController._

  override protected implicit val writes: Writes[ReportCardRetryLike] = ReportCardRetryLike.writes

  override protected implicit val reads: Reads[ReportCardRetryProtocol] = ReportCardRetryProtocol.reads

  override protected def makeTableFilter(attribute: String, value: String): Try[TableFilterPredicate] =
    (attribute, value) match {
      case (`reportCardEntryAttribute`, e) => e.makeReportCardEntryFilter
      case (`studentAttribute`, s) => s.makeUserByReportCardEntryFilter
      case (`labworkAttribute`, l) => l.makeLabworkByReportCardEntryFilter
      case (`courseAttribute`, c) => c.makeCourseByReportCardEntryFilter
      case (`roomAttribute`, r) => r.makeRoomByReportCardEntryFilter
      case _ => makeTimeRangeFilter(attribute, value)
    }

  override protected def toDbModel(protocol: ReportCardRetryProtocol, existingId: Option[UUID]): ReportCardRetryDb = {
    val uuid = existingId getOrElse UUID.randomUUID
    val entryTypes = protocol.entryTypes.map(t => ReportCardEntryTypeDb(None, Some(uuid), t.entryType, t.bool, t.int))

    ReportCardRetryDb(
      protocol.reportCardEntry,
      protocol.date.sqlDate,
      protocol.start.sqlTime,
      protocol.end.sqlTime,
      protocol.room, entryTypes,
      protocol.reason, id = uuid
    )
  }

  def createFrom(course: String) = restrictedContext(course)(Create) asyncAction { request =>
    create(NonSecureBlock)(request)
  }

  def allFrom(course: String) = restrictedContext(course)(GetAll) asyncAction { request =>
    all(NonSecureBlock)(request.appending(courseAttribute -> Seq(course)))
  }

  def getFrom(course: String, id: String) = restrictedContext(course)(Get) asyncAction { request =>
    get(id, NonSecureBlock)(request)
  }

  def updateFrom(course: String, id: String) = restrictedContext(course)(Update) asyncAction { request =>
    update(id, NonSecureBlock)(request)
  }

  def invalidateFrom(course: String, id: String) = restrictedContext(course)(Delete) asyncAction { request =>
    invalidate(id, NonSecureBlock)(request)
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = forbiddenAction()

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case _ => SecureBlock(restrictionId, List(CourseManager, CourseEmployee))
  }
}