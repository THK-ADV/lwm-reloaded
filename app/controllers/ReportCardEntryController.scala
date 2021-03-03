package controllers

import controllers.helper.TimeRangeTableFilter
import dao._
import dao.helper.TableFilter.labworkFilter
import database.{ReportCardEntryDb, ReportCardEntryTable}
import models._
import play.api.libs.json.{Json, Reads, Writes}
import play.api.mvc.ControllerComponents
import security.LWMRole._
import security.SecurityActionChain
import service.ReportCardEntryService.ReportCardEntryDescription
import service._
import service.sheet.{FileStreamResult, ReportCardEntryExport}

import java.util.UUID
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object ReportCardEntryController {
  lazy val studentAttribute = "student"
  lazy val courseAttribute = "course"
  lazy val labworkAttribute = "labwork"
  lazy val roomAttribute = "room"
  lazy val scheduleEntryAttribute = "scheduleEntry"
  lazy val semesterAttribute = "semester"
}

@Singleton
final class ReportCardEntryController @Inject()(
  cc: ControllerComponents,
  val authorityDao: AuthorityDao,
  val service: ReportCardEntryService,
  val securedAction: SecurityActionChain,
  implicit val ctx: ExecutionContext
) extends AbstractCRUDController[ReportCardEntryProtocol, ReportCardEntryTable, ReportCardEntryDb, ReportCardEntryLike](cc)
  with TimeRangeTableFilter[ReportCardEntryTable]
  with FileStreamResult {

  import controllers.ReportCardEntryController._
  import utils.date.DateTimeJsonFormatter._

  override protected implicit val writes: Writes[ReportCardEntryLike] = ReportCardEntryLike.writes

  override protected implicit val reads: Reads[ReportCardEntryProtocol] = ReportCardEntryProtocol.reads

  override protected def toDbModel(protocol: ReportCardEntryProtocol, existingId: Option[UUID]): ReportCardEntryDb = ???

  def getForStudent(student: String) = contextFrom(Get) asyncAction { request =>
    all(NonSecureBlock)(request.appending(studentAttribute -> Seq(student)))
  }

  def allFrom(course: String) = restrictedContext(course)(GetAll) asyncAction { request =>
    all(NonSecureBlock)(request.appending(courseAttribute -> Seq(course)))
  }

  def fromScheduleEntry(course: String, scheduleEntry: String) = restrictedContext(course)(GetAll) asyncAction { request =>
    val requestWithScheduleEntry = request.appending(scheduleEntryAttribute -> Seq(scheduleEntry))

    allWithFilter { (filter, defaults) =>
      service
        .withAnnotationCountInLabwork(filter, defaults.atomic, defaults.valid, defaults.lastModified)
        .jsonResult
    }(requestWithScheduleEntry)
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

  def rescheduleCandidates(course: String, semester: String) = restrictedContext(course)(GetAll) asyncAction { request =>
    (for {
      cid <- course.uuidF
      sid <- semester.uuidF
      candidates <- service.rescheduleCandidates(cid, sid)
    } yield candidates.map {
      case (date, start, end, room, members) => Json.obj(
        "date" -> date,
        "start" -> start,
        "end" -> end,
        "room" -> room,
        "members" -> members
      )
    }).jsonResult
  }

  def extend(course: String, labwork: String) = restrictedContext(course)(Create) asyncAction { implicit request =>
    implicit val reads: Reads[ReportCardEntryDescription] = Json.reads[ReportCardEntryDescription]

    (for {
      descriptions <- Future.fromTry(parseJsonArray(request)(listReads(reads)))
      labworkId <- labwork.uuidF
      atomic = isAtomic(default = true)
      cards <- service.extendBy(labworkId, descriptions, atomic)
    } yield cards).jsonResult
  }

  def renderStatSheet(course: String, labwork: String) = restrictedContext(course)(Create) asyncAction { _ =>
    import utils.Ops.whenNonEmpty

    for {
      labworkId <- labwork.uuidF
      allCards <- whenNonEmpty(abstractDao.get(List(labworkFilter(labworkId))))(() => "no report cards found")
      allCards0 = allCards.map(_.asInstanceOf[ReportCardEntryAtom]).toList
      labwork = allCards0.head.labwork
      sheet = ReportCardEntryExport.createSheet(allCards0, labwork)
    } yield toResult(sheet)
  }

  override protected def makeTableFilter(attribute: String, value: String): Try[TableFilterPredicate] = {
    import ReportCardEntryController._
    import ReportCardEntryDao._
    import dao.helper.TableFilter.semesterFilter

    (attribute, value) match {
      case (`studentAttribute`, s) => s.makeUserFilter
      case (`courseAttribute`, c) => c.makeCourseFilter
      case (`labworkAttribute`, l) => l.makeLabworkFilter
      case (`roomAttribute`, r) => r.makeRoomFilter
      case (`scheduleEntryAttribute`, s) => s.uuid map scheduleEntryFilter
      case (`semesterAttribute`, s) => s.uuid map semesterFilter
      case _ => makeTimeRangeFilter(attribute, value)
    }
  }

  override protected val abstractDao: AbstractDao[ReportCardEntryTable, ReportCardEntryDb, ReportCardEntryLike] = service.dao

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(List(StudentRole))
    case _ => PartialSecureBlock(List(God))
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case Create | Delete => SecureBlock(restrictionId, List(CourseManager))
    case Update => SecureBlock(restrictionId, List(CourseManager, CourseEmployee))
    case GetAll => SecureBlock(restrictionId, List(CourseManager, CourseEmployee, CourseAssistant))
    case _ => PartialSecureBlock(List(God))
  }
}