package controllers

import java.util.UUID

import dao._
import database.{TimetableDb, TimetableTable}
import javax.inject.{Inject, Singleton}
import models.Role.{CourseAssistant, CourseEmployee, CourseManager}
import models.{TimetableLike, TimetableProtocol}
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.ControllerComponents
import security.SecurityActionChain
import service.TimetableService

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Try}

object TimetableController {
  lazy val courseAttribute = "course"
  lazy val labworkAttribute = "labwork"
}

@Singleton
final class TimetableController @Inject()(
  cc: ControllerComponents,
  val authorityDao: AuthorityDao,
  val abstractDao: TimetableDao,
  val blacklistDao: BlacklistDao,
  val securedAction: SecurityActionChain,
  implicit val ctx: ExecutionContext
)
  extends AbstractCRUDController[TimetableProtocol, TimetableTable, TimetableDb, TimetableLike](cc) {

  override protected implicit val writes: Writes[TimetableLike] = TimetableLike.writes

  override protected implicit val reads: Reads[TimetableProtocol] = TimetableProtocol.reads

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case Create => SecureBlock(restrictionId, List(CourseManager))
    case Get => SecureBlock(restrictionId, List(CourseManager, CourseEmployee, CourseAssistant))
    case GetAll => SecureBlock(restrictionId, List(CourseManager, CourseEmployee))
    case Update => SecureBlock(restrictionId, List(CourseManager))
    case Delete => SecureBlock(restrictionId, List(CourseManager))
  }

  def createFrom(course: String) = restrictedContext(course)(Create) asyncAction { request =>
    create(NonSecureBlock)(request)
  }

  def updateFrom(course: String, id: String) = restrictedContext(course)(Update) asyncAction { request =>
    update(id, NonSecureBlock)(request)
  }

  def deleteFrom(course: String, id: String) = restrictedContext(course)(Delete) asyncAction { request =>
    delete(id, NonSecureBlock)(request)
  }

  def allFrom(course: String) = restrictedContext(course)(GetAll) asyncAction { request =>
    import controllers.TimetableController.courseAttribute

    all(NonSecureBlock)(request.appending(courseAttribute -> Seq(course)))
  }

  def getFrom(course: String, id: String) = restrictedContext(course)(Get) asyncAction { request =>
    get(id, NonSecureBlock)(request)
  }

  def removeBlacklistFrom(course: String, timetableId: String, blacklistId: String) = restrictedContext(course)(Delete) asyncAction { _ =>
    (for {
      tid <- timetableId.uuidF
      bid <- blacklistId.uuidF
      timetable <- TimetableService.removeBlacklistFromTimetable(blacklistDao, abstractDao)(bid, tid)
    } yield timetable).jsonResult
  }

  override protected def makeTableFilter(attribute: String, value: String): Try[TableFilterPredicate] = {
    import TimetableController._

    (attribute, value) match {
      case (`courseAttribute`, c) => c.makeCourseFilter
      case (`labworkAttribute`, l) => l.makeLabworkFilter
      case _ => Failure(new Throwable(s"Unknown attribute $attribute"))
    }
  }

  override protected def toDbModel(protocol: TimetableProtocol, existingId: Option[UUID]): TimetableDb = {
    import utils.date.DateTimeOps.LocalDateConverter
    TimetableDb(protocol.labwork, protocol.entries, protocol.start.sqlDate, protocol.localBlacklist, id = existingId getOrElse UUID.randomUUID)
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = forbiddenAction()
}
