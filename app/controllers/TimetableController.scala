package controllers

import dao._
import database.{TimetableDb, TimetableTable}
import models.{TimetableLike, TimetableProtocol}
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.ControllerComponents
import security.LWMRole.{CourseAssistant, CourseEmployee, CourseManager}
import security.SecurityActionChain
import service.TimetableService

import java.util.UUID
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}
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
) extends AbstractCRUDController[TimetableProtocol, TimetableTable, TimetableDb, TimetableLike](cc) {

  import TimetableController._
  import utils.date.DateTimeOps.LocalDateConverter

  override protected implicit val writes: Writes[TimetableLike] = TimetableLike.writes

  override protected implicit val reads: Reads[TimetableProtocol] = TimetableProtocol.reads

  def createFrom(course: String) = restrictedContext(course)(Create) asyncAction { request =>
    create(NonSecureBlock)(request)
  }

  def updateFrom(course: String, id: String) = restrictedContext(course)(Update) asyncAction { request =>
    update(id, NonSecureBlock)(request)
  }

  def invalidateFrom(course: String, id: String) = restrictedContext(course)(Delete) asyncAction { request =>
    invalidate(id, NonSecureBlock)(request)
  }

  def allFrom(course: String) = restrictedContext(course)(GetAll) asyncAction { request =>
    import controllers.TimetableController.courseAttribute

    all(NonSecureBlock)(request.appending(courseAttribute -> Seq(course)))
  }

  def getFrom(course: String, id: String) = restrictedContext(course)(Get) asyncAction { request =>
    get(id, NonSecureBlock)(request)
  }

  def removeBlacklistFrom(course: String, timetableId: String, blacklistId: String) = restrictedContext(course)(Delete) asyncAction { implicit request =>
    (for {
      tid <- timetableId.uuidF
      bid <- blacklistId.uuidF
      timetable <- TimetableService.removeBlacklistFromTimetable(blacklistDao, abstractDao)(bid, tid)
      atomic = isAtomic(default = true)
      tt <- if (atomic)
        abstractDao.getSingle(timetable._1.id, atomic)
      else
        Future.successful(Some(timetable._1))
      if tt.isDefined
    } yield tt.get).jsonResult
  }

  override protected def makeTableFilter(attribute: String, value: String): Try[TableFilterPredicate] =
    (attribute, value) match {
      case (`courseAttribute`, c) => c.makeCourseFilter
      case (`labworkAttribute`, l) => l.makeLabworkFilter
      case _ => Failure(new Throwable(s"Unknown attribute $attribute"))
    }

  override protected def toDbModel(protocol: TimetableProtocol, existingId: Option[UUID]): TimetableDb =
    TimetableDb(
      protocol.labwork,
      protocol.entries,
      protocol.start.sqlDate,
      protocol.localBlacklist,
      id = existingId getOrElse UUID.randomUUID
    )

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = forbiddenAction()

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case Get => SecureBlock(restrictionId, List(CourseManager, CourseEmployee, CourseAssistant))
    case GetAll => SecureBlock(restrictionId, List(CourseManager, CourseEmployee))
    case Create | Update | Delete => SecureBlock(restrictionId, List(CourseManager))
  }
}
