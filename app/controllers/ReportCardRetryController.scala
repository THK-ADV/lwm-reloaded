package controllers

import java.util.UUID

import dao._
import javax.inject.{Inject, Singleton}
import models.Role.{CourseEmployee, CourseManager}
import models._
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.ControllerComponents
import database.{ReportCardEntryTypeDb, ReportCardRetryDb, ReportCardRetryTable, TableFilter}
import utils.LwmDateTime._
import utils.SecuredAction

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

@Singleton
final class ReportCardRetryController @Inject()(cc: ControllerComponents, val authorityDao: AuthorityDao, val abstractDao: ReportCardRetryDao, val securedAction: SecuredAction)
  extends AbstractCRUDController[ReportCardRetryProtocol, ReportCardRetryTable, ReportCardRetryDb, ReportCardRetryLike](cc) {

  import controllers.ReportCardRetryController._

  override protected implicit val writes: Writes[ReportCardRetryLike] = ReportCardRetryLike.writes

  override protected implicit val reads: Reads[ReportCardRetryProtocol] = ReportCardRetryProtocol.reads

  override protected def tableFilter(attribute: String, value: String)(appendTo: Try[List[TableFilter[ReportCardRetryTable]]]): Try[List[TableFilter[ReportCardRetryTable]]] = {
    (appendTo, (attribute, value)) match { // TODO more attributes
      case (list, (`reportCardEntryAttribute`, reportCardEntry)) => list.map(_.+:(ReportCardRetryEntryFilter(reportCardEntry)))
      case (list, (`labworkAttribute`, labwork)) => list.map(_.+:(ReportCardRetryLabworkFilter(labwork)))
      case (list, (`courseAttribute`, course)) => list.map(_.+:(ReportCardRetryCourseFilter(course)))
      case _ => Failure(new Throwable("Unknown attribute"))
    }
  }

  override protected def toDbModel(protocol: ReportCardRetryProtocol, existingId: Option[UUID]): ReportCardRetryDb = {
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

  def deleteFrom(course: String, id: String) = restrictedContext(course)(Delete) asyncAction { request =>
    delete(id, NonSecureBlock)(request)
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = forbidden()
}