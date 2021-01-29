package controllers

import controllers.helper.TimeRangeTableFilter
import dao._
import database.{ReportCardRescheduledDb, ReportCardRescheduledTable}
import models.Role.{CourseEmployee, CourseManager}
import models.{ReportCardRescheduledLike, ReportCardRescheduledProtocol}
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.ControllerComponents
import security.SecurityActionChain
import utils.date.DateTimeOps._

import java.util.UUID
import javax.inject.{Inject, Singleton}
import scala.util.Try

object ReportCardRescheduledController {
  lazy val reportCardEntryAttribute = "reportCardEntry"
  lazy val studentAttribute = "student"
  lazy val labworkAttribute = "labwork"
  lazy val courseAttribute = "course"
  lazy val roomAttribute = "room"
}

@Singleton
final class ReportCardRescheduledController @Inject()(cc: ControllerComponents, val authorityDao: AuthorityDao, val abstractDao: ReportCardRescheduledDao, val securedAction: SecurityActionChain)
  extends AbstractCRUDController[ReportCardRescheduledProtocol, ReportCardRescheduledTable, ReportCardRescheduledDb, ReportCardRescheduledLike](cc)
    with TimeRangeTableFilter[ReportCardRescheduledTable] {

  import controllers.ReportCardRescheduledController._

  override protected implicit val writes: Writes[ReportCardRescheduledLike] = ReportCardRescheduledLike.writes

  override protected implicit val reads: Reads[ReportCardRescheduledProtocol] = ReportCardRescheduledProtocol.reads

  override protected def makeTableFilter(attribute: String, value: String): Try[TableFilterPredicate] = {
    import ReportCardRescheduledController._

    (attribute, value) match {
      case (`reportCardEntryAttribute`, e) => e.makeReportCardEntryFilter
      case (`studentAttribute`, s) => s.makeUserByReportCardEntryFilter
      case (`labworkAttribute`, l) => l.makeLabworkByReportCardEntryFilter
      case (`courseAttribute`, c) => c.makeCourseByReportCardEntryFilter
      case (`roomAttribute`, r) => r.makeRoomByReportCardEntryFilter
      case _ => makeTimeRangeFilter(attribute, value)
    }
  }

  override protected def toDbModel(protocol: ReportCardRescheduledProtocol, existingId: Option[UUID]): ReportCardRescheduledDb = {
    ReportCardRescheduledDb(protocol.reportCardEntry, protocol.date.sqlDate, protocol.start.sqlTime, protocol.end.sqlTime, protocol.room, protocol.reason, id = existingId getOrElse UUID.randomUUID)
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case Create => SecureBlock(restrictionId, List(CourseManager, CourseEmployee))
    case Delete => SecureBlock(restrictionId, List(CourseManager, CourseEmployee))
    case GetAll => SecureBlock(restrictionId, List(CourseManager, CourseEmployee))
    case Get => SecureBlock(restrictionId, List(CourseManager, CourseEmployee))
    case Update => SecureBlock(restrictionId, List(CourseManager, CourseEmployee))
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
}
