package controllers

import java.util.UUID

import controllers.core.AbstractCRUDController
import controllers.helper.TimeRangeTableFilter
import dao._
import database.{ReportCardEntryDb, ReportCardEntryTable}
import javax.inject.{Inject, Singleton}
import models.Role._
import models._
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.ControllerComponents
import security.SecurityActionChain
import service._

import scala.concurrent.ExecutionContext
import scala.util.Try

object ReportCardEntryController {
  lazy val studentAttribute = "student"
  lazy val courseAttribute = "course"
  lazy val labworkAttribute = "labwork"
  lazy val roomAttribute = "room"
  lazy val scheduleEntryAttribute = "scheduleEntry"
}

@Singleton
final class ReportCardEntryController @Inject()(
  cc: ControllerComponents,
  val authorityDao: AuthorityDao,
  val service: ReportCardEntryService,
  val securedAction: SecurityActionChain,
  implicit val ctx: ExecutionContext
) extends AbstractCRUDController[ReportCardEntryProtocol, ReportCardEntryTable, ReportCardEntryDb, ReportCardEntryLike](cc)
  with TimeRangeTableFilter[ReportCardEntryTable] {

  import controllers.ReportCardEntryController._
  import controllers.core.DBFilterOps._

  override protected implicit val writes: Writes[ReportCardEntryLike] = ReportCardEntryLike.writes

  override protected implicit val reads: Reads[ReportCardEntryProtocol] = ReportCardEntryProtocol.reads

  override protected def toDbModel(protocol: ReportCardEntryProtocol, existingId: Option[UUID]): ReportCardEntryDb = ???

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(List(StudentRole, CourseAssistant))
    case _ => PartialSecureBlock(List(God))
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case Create => SecureBlock(restrictionId, List(CourseManager))
    case Delete => SecureBlock(restrictionId, List(CourseManager))
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

  def invalidateFrom(course: String, labwork: String) = restrictedContext(course)(Delete) asyncAction { _ =>
    labwork
      .uuidF
      .flatMap(service.dao.invalidateByLabwork)
      .map(_.map(_.toUniqueEntity))
      .jsonResult
  }

  def createFrom(course: String, labwork: String) = restrictedContext(course)(Create) asyncAction { request =>
    labwork
      .uuidF
      .flatMap(service.generate)
      .jsonResult
  }

  def countFrom(course: String, labwork: String) = restrictedContext(course)(GetAll) asyncAction { _ =>
    course.uuidF
      .zip(labwork.uuidF)
      .flatMap(t => service.dao.numberOfStudents(t._1, t._2))
      .jsonResult
  }

  override protected def makeTableFilter(attribute: String, value: String): Try[TableFilterPredicate] = {
    import ReportCardEntryController._
    import ReportCardEntryDao._

    (attribute, value) match {
      case (`studentAttribute`, s) => s.makeUserFilter
      case (`courseAttribute`, c) => c.makeCourseFilter
      case (`labworkAttribute`, l) => l.makeLabworkFilter
      case (`roomAttribute`, r) => r.makeRoomFilter
      case (`scheduleEntryAttribute`, s) => s.uuid map scheduleEntryFilter
      case _ => makeTimeRangeFilter(attribute, value)
    }
  }

  override protected val abstractDao: AbstractDao[ReportCardEntryTable, ReportCardEntryDb, ReportCardEntryLike] = service.dao
}