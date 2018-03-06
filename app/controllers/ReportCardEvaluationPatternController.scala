package controllers

import java.util.UUID

import dao._
import models.Role.CourseManager
import models.{ReportCardEvaluationPattern, ReportCardEvaluationPatternDb, ReportCardEvaluationPatternProtocol}
import play.api.libs.json.{Reads, Writes}
import services.SessionHandlingService
import store.{ReportCardEvaluationPatternTable, TableFilter}
import utils.LwmMimeType

import scala.util.{Failure, Try}

object ReportCardEvaluationPatternController {
  lazy val courseAttribute = "course"
  lazy val labworkAttribute = "labwork"
  lazy val entryTypeAttribute = "entryType"
}


final class ReportCardEvaluationPatternController(val authorityDao: AuthorityDao,
                                                  val sessionService: SessionHandlingService,
                                                  val abstractDao: ReportCardEvaluationPatternDao)
  extends AbstractCRUDControllerPostgres[ReportCardEvaluationPatternProtocol, ReportCardEvaluationPatternTable, ReportCardEvaluationPatternDb, ReportCardEvaluationPattern] {

  override protected implicit val writes: Writes[ReportCardEvaluationPattern] = ReportCardEvaluationPattern.writes

  override protected implicit val reads: Reads[ReportCardEvaluationPatternProtocol] = ReportCardEvaluationPatternProtocol.reads

  def createFrom(course: String) = restrictedContext(course)(Create) asyncContentTypedAction { request =>
    create(NonSecureBlock)(request)
  }

  def updateFrom(course: String, id: String) = restrictedContext(course)(Update) asyncContentTypedAction { request =>
    update(id, NonSecureBlock)(request)
  }

  def deleteFrom(course: String, id: String) = restrictedContext(course)(Delete) asyncAction { request =>
    delete(id, NonSecureBlock)(request)
  }

  def allFrom(course: String) = restrictedContext(course)(GetAll) asyncAction { request =>
    import controllers.ReportCardEvaluationPatternController.courseAttribute

    all(NonSecureBlock)(request.append(courseAttribute -> Seq(course)))
  }

  def getFrom(course: String, id: String) = restrictedContext(course)(Get) asyncAction { request =>
    get(id, NonSecureBlock)(request)
  }

  override implicit val mimeType: LwmMimeType = LwmMimeType.reportCardEvaluationPatternV1Json

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case Create => SecureBlock(restrictionId, List(CourseManager))
    case Get => SecureBlock(restrictionId, List(CourseManager))
    case GetAll => SecureBlock(restrictionId, List(CourseManager))
    case Update => SecureBlock(restrictionId, List(CourseManager))
    case Delete => SecureBlock(restrictionId, List(CourseManager))
  }

  override protected def tableFilter(attribute: String, value: String)(appendTo: Try[List[TableFilter[ReportCardEvaluationPatternTable]]]): Try[List[TableFilter[ReportCardEvaluationPatternTable]]] = {
    import controllers.ReportCardEvaluationPatternController._

    (appendTo, (attribute, value)) match {
      case (list, (`courseAttribute`, course)) => list.map(_.+:(EvaluationPatternCourseFilter(course)))
      case (list, (`labworkAttribute`, labwork)) => list.map(_.+:(EvaluationPatternLabworkFilter(labwork)))
      case (list, (`entryTypeAttribute`, entryType)) => list.map(_.+:(EvaluationPatternEntryTypeFilter(entryType)))
      case _ => Failure(new Throwable("Unknown attribute"))
    }
  }

  override protected def toDbModel(protocol: ReportCardEvaluationPatternProtocol, existingId: Option[UUID]): ReportCardEvaluationPatternDb = {
    ReportCardEvaluationPatternDb.from(protocol, existingId)
  }
}
