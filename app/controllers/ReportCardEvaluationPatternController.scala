package controllers

import java.util.UUID

import controllers.core.AbstractCRUDController
import dao._
import database.{ReportCardEvaluationPatternDb, ReportCardEvaluationPatternTable}
import javax.inject.{Inject, Singleton}
import models.Role.CourseManager
import models.{ReportCardEvaluationPattern, ReportCardEvaluationPatternProtocol}
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.ControllerComponents
import security.SecurityActionChain

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Try}

object ReportCardEvaluationPatternController {
  lazy val courseAttribute = "course"
  lazy val labworkAttribute = "labwork"
  lazy val entryTypeAttribute = "entryType"
}

@Singleton
final class ReportCardEvaluationPatternController @Inject()(
  cc: ControllerComponents,
  val authorityDao: AuthorityDao,
  val abstractDao: ReportCardEvaluationPatternDao,
  val securedAction: SecurityActionChain,
  implicit val ctx: ExecutionContext
) extends AbstractCRUDController[ReportCardEvaluationPatternProtocol, ReportCardEvaluationPatternTable, ReportCardEvaluationPatternDb, ReportCardEvaluationPattern](cc) {

  import controllers.core.DBFilterOps._

  override protected implicit val writes: Writes[ReportCardEvaluationPattern] = ReportCardEvaluationPattern.writes

  override protected implicit val reads: Reads[ReportCardEvaluationPatternProtocol] = ReportCardEvaluationPatternProtocol.reads

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
    import controllers.ReportCardEvaluationPatternController.courseAttribute

    all(NonSecureBlock)(request.appending(courseAttribute -> Seq(course)))
  }

  def getFrom(course: String, id: String) = restrictedContext(course)(Get) asyncAction { request =>
    get(id, NonSecureBlock)(request)
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case Create => SecureBlock(restrictionId, List(CourseManager))
    case Get => SecureBlock(restrictionId, List(CourseManager))
    case GetAll => SecureBlock(restrictionId, List(CourseManager))
    case Update => SecureBlock(restrictionId, List(CourseManager))
    case Delete => SecureBlock(restrictionId, List(CourseManager))
  }

  override protected def makeTableFilter(attribute: String, value: String): Try[TableFilterPredicate] = {
    import ReportCardEvaluationPatternController._

    (attribute, value) match {
      case (`courseAttribute`, c) => c.makeCourseFilter
      case (`labworkAttribute`, l) => l.makeLabworkFilter
      case (`entryTypeAttribute`, e) => e.makeEntryTypeFilter
      case _ => Failure(new Throwable(s"Unknown attribute $attribute"))
    }
  }

  override protected def toDbModel(protocol: ReportCardEvaluationPatternProtocol, existingId: Option[UUID]): ReportCardEvaluationPatternDb = {
    ReportCardEvaluationPatternDb(protocol.labwork, protocol.entryType, protocol.min, protocol.property.toString, id = existingId getOrElse UUID.randomUUID)
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = forbiddenAction()
}
