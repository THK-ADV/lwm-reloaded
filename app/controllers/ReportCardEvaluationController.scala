package controllers

import java.util.UUID

import dao._
import dao.helper.TableFilterable
import database.{ReportCardEvaluationDb, ReportCardEvaluationTable}
import javax.inject.{Inject, Singleton}
import models.Role._
import models.{ReportCardEvaluationLike, ReportCardEvaluationProtocol}
import play.api.libs.json._
import play.api.mvc.{AnyContent, ControllerComponents, Request, Result}
import security.SecurityActionChain
import services.ReportCardService

import scala.concurrent.Future

object ReportCardEvaluationController {
  lazy val courseAttribute = "course"
  lazy val labworkAttribute = "labwork"
  lazy val studentAttribute = "student"

  lazy val labelAttribute = "label"
  lazy val boolAttribute = "bool"
  lazy val intAttribute = "int"
  lazy val minIntAttribute = "minInt"
  lazy val maxIntAttribute = "maxInt"
  lazy val explicitAttribute = "explicit"
}

@Singleton
final class ReportCardEvaluationController @Inject()(cc: ControllerComponents, val authorityDao: AuthorityDao, val abstractDao: ReportCardEvaluationDao, val reportCardEntryDao: ReportCardEntryDao, val reportCardEvaluationPatternDao: ReportCardEvaluationPatternDao, val securedAction: SecurityActionChain)
  extends AbstractCRUDController[ReportCardEvaluationProtocol, ReportCardEvaluationTable, ReportCardEvaluationDb, ReportCardEvaluationLike](cc) {

  import TableFilterable.{labworkFilter, studentFilter}
  import controllers.ReportCardEvaluationController._

  import scala.concurrent.ExecutionContext.Implicits.global

  def get(student: String) = contextFrom(Get) asyncAction { request =>
    all(NonSecureBlock)(request.appending(studentAttribute -> Seq(student)))
  }

  def createFrom(course: String, labwork: String) = restrictedContext(course)(Create) asyncAction { implicit request =>
    evaluate(labwork, persistence = true)
  }

  def createForStudent(course: String, labwork: String, student: String) = restrictedContext(course)(Create) asyncAction { _ =>
    for {
      labworkId <- labwork.uuidF
      studentId <- student.uuidF
      existing <- abstractDao.get(List(labworkFilter(labworkId), studentFilter(studentId)), atomic = false)
      result <- if (existing.isEmpty)
        abstractDao.createMany(ReportCardService.evaluateExplicit(UUID.fromString(student), UUID.fromString(labwork))).map(_.map(_.toUniqueEntity)).jsonResult
      else
        Future.successful(preconditionFailed(s"$student was already evaluated: ${Json.toJson(existing)}. delete those first before continuing with explicit evaluation."))
    } yield result
  }

  def preview(course: String, labwork: String) = restrictedContext(course)(GetAll) asyncAction { implicit request =>
    evaluate(labwork, persistence = false)
  }

  def allFrom(course: String, labwork: String) = restrictedContext(course)(GetAll) asyncAction { request =>
    all(NonSecureBlock)(request.appending(courseAttribute -> Seq(course), labworkAttribute -> Seq(labwork)))
  }

  def deleteFromStudent(course: String, labwork: String, student: String) = restrictedContext(course)(Delete) asyncAction { _ =>
    for {
      labworkId <- labwork.uuidF
      studentId <- student.uuidF
      result <- delete(List(labworkFilter(labworkId), studentFilter(studentId)))
    } yield result
  }

  def deleteFromLabwork(course: String, labwork: String) = restrictedContext(course)(Delete) asyncAction { _ =>
    for {
      labworkId <- labwork.uuidF
      result <- delete(List(labworkFilter(labworkId)))
    } yield result
  }

  def updateFrom(course: String, labwork: String, id: String) = restrictedContext(course)(Update) asyncAction { request =>
    update(id, NonSecureBlock)(request)
  }

  private def delete(list: List[TableFilterPredicate]): Future[Result] = {
    (for {
      existing <- abstractDao.get(list, atomic = false)
      deleted <- abstractDao.deleteMany(existing.map(_.id).toList)
    } yield deleted).deleted
  }

  private def evaluate(labwork: String, persistence: Boolean)(implicit request: Request[AnyContent]) = {
    (for {
      labworkId <- labwork.uuidF
      patterns <- reportCardEvaluationPatternDao.get(List(labworkFilter(labworkId)), atomic = false) if patterns.nonEmpty
      cards <- reportCardEntryDao.get(List(labworkFilter(labworkId)), atomic = false)
      existing <- abstractDao.get(List(labworkFilter(labworkId)), atomic = false)

      evaluated = ReportCardService.evaluateDeltas(cards.toList, patterns.toList, existing.toList)
      _ <- if (persistence) abstractDao.createOrUpdateMany(evaluated) else Future.successful(Seq.empty)

      atomic = extractAttributes(request.queryString, defaultAtomic = false)._2.atomic
      evaluations <- if (atomic) abstractDao.getMany(evaluated.map(_.id), atomic) else Future.successful(evaluated.map(_.toUniqueEntity))
    } yield evaluations).jsonResult
  }

  override protected implicit val writes: Writes[ReportCardEvaluationLike] = ReportCardEvaluationLike.writes

  override protected implicit val reads: Reads[ReportCardEvaluationProtocol] = ReportCardEvaluationProtocol.reads

  //  override protected def tableFilter(attribute: String, value: String)(appendTo: Try[List[TableFilter[ReportCardEvaluationTable]]]): Try[List[TableFilter[ReportCardEvaluationTable]]] = {
  //    (appendTo, (attribute, value)) match {
  //      case (list, (`courseAttribute`, course)) => list.map(_.+:(CourseFilter(course)))
  //      case (list, (`labworkAttribute`, labwork)) => list.map(_.+:(LabworkFilter(labwork)))
  //      case (list, (`studentAttribute`, student)) => list.map(_.+:(StudentFilter(student)))
  //      case (list, (`labelAttribute`, label)) => list.map(_.+:(LabelFilter(label)))
  //      case (list, (`boolAttribute`, bool)) => list.map(_.+:(BoolFilter(bool)))
  //      case (list, (`intAttribute`, int)) => list.map(_.+:(IntFilter(int)))
  //      case (list, (`minIntAttribute`, minInt)) => list.map(_.+:(MinIntFilter(minInt)))
  //      case (list, (`maxIntAttribute`, maxInt)) => list.map(_.+:(MaxIntFilter(maxInt)))
  //      case (list, (`explicitAttribute`, explicit)) if explicit == true.toString => list.map(_.+:(IntFilter(ReportCardService.EvaluatedExplicit.toString)))
  //      case (_, (`explicitAttribute`, other)) => Failure(new Throwable(s"Value of $explicitAttribute can only be true, but was $other"))
  //      case _ => Failure(new Throwable("Unknown attribute"))
  //    }
  //  }

  override protected def toDbModel(protocol: ReportCardEvaluationProtocol, existingId: Option[UUID]): ReportCardEvaluationDb = {
    ReportCardEvaluationDb.from(protocol, existingId)
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case Create => SecureBlock(restrictionId, List(CourseManager))
    case Get => SecureBlock(restrictionId, List(CourseManager))
    case Delete => SecureBlock(restrictionId, List(CourseManager))
    case Update => SecureBlock(restrictionId, List(CourseManager))
    case GetAll => SecureBlock(restrictionId, List(CourseManager, CourseEmployee))
    case _ => PartialSecureBlock(List(God))
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(List(StudentRole))
    case _ => PartialSecureBlock(List(God))
  }
}
