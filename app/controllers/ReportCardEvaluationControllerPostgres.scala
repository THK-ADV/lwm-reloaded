package controllers

import java.util.UUID

import dao._
import models.{ReportCardEvaluation, ReportCardEvaluationDb}
import play.api.libs.json._
import services.ReportCardService.{BoolBased, IntBased, ReportCardEvaluationPattern}
import services.{ReportCardService, SessionHandlingService}
import store.{ReportCardEvaluationTable, TableFilter}
import utils.LwmMimeType
import play.api.libs.functional.syntax._
import models.Role._
import play.api.mvc.Request

import scala.concurrent.Future
import scala.util.{Failure, Try}

object ReportCardEvaluationControllerPostgres {
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

final class ReportCardEvaluationControllerPostgres(val authorityDao: AuthorityDao,
                                                   val sessionService: SessionHandlingService,
                                                   val abstractDao: ReportCardEvaluationDao,
                                                   val reportCardEntryDao: ReportCardEntryDao
                                                  ) extends AbstractCRUDControllerPostgres[ReportCardEvaluationPattern, ReportCardEvaluationTable, ReportCardEvaluationDb, ReportCardEvaluation] {

  import controllers.ReportCardEvaluationControllerPostgres._
  import scala.concurrent.ExecutionContext.Implicits.global

  def get(student: String) = contextFrom(Get) asyncAction { request =>
    all(NonSecureBlock)(request.append(studentAttribute -> Seq(student)))
  }

  def createFrom(course: String, labwork: String) = restrictedContext(course)(Create) asyncContentTypedAction { implicit request =>
    evaluate(labwork, persistence = true)
  }

  def createForStudent(course: String, labwork: String, student: String) = restrictedContext(course)(Create) asyncContentTypedAction { _ =>
    for {
      existing <- abstractDao.get(List(LabworkFilter(labwork), StudentFilter(student)), atomic = false)
      result <- if (existing.isEmpty)
        abstractDao.createMany(ReportCardService.evaluateExplicit(UUID.fromString(student), UUID.fromString(labwork))).map(_.map(_.toLwmModel)).jsonResult
      else
        Future.successful(preconditionFailed(s"$student was already evaluated: ${Json.toJson(existing)}. delete those first before continuing with explicit evaluation."))
    } yield result
  }

  def preview(course: String, labwork: String) = restrictedContext(course)(GetAll) asyncContentTypedAction { implicit request =>
    evaluate(labwork, persistence = false)
  }

  def allFrom(course: String, labwork: String) = restrictedContext(course)(GetAll) asyncAction { request =>
    all(NonSecureBlock)(request.append(courseAttribute -> Seq(course), labworkAttribute -> Seq(labwork)))
  }

  def deleteFromStudent(course: String, labwork: String, student: String) = restrictedContext(course)(Delete) asyncAction { _ =>
    delete(List(LabworkFilter(labwork), StudentFilter(student)))
  }

  def deleteFromLabwork(course: String, labwork: String) = restrictedContext(course)(Delete) asyncAction { _ =>
    delete(List(LabworkFilter(labwork)))
  }

  private def delete(list: List[TableFilter[ReportCardEvaluationTable]]) = {
    import utils.LwmDateTime._

    (for {
      existing <- abstractDao.get(list, atomic = false)
      deleted <- abstractDao.deleteMany(existing.map(_.id).toList)
    } yield deleted.map(_.map(_.dateTime))).jsonResult
  }

  private def evaluate(labwork: String, persistence: Boolean)(implicit request: Request[JsValue]) = {
    (for {
      patterns <- Future.fromTry(parseArray[ReportCardEvaluationPattern](request))
      cards <- reportCardEntryDao.get(List(ReportCardEntryLabworkFilter(labwork)), atomic = false)
      existing <- abstractDao.get(List(LabworkFilter(labwork)), atomic = false)

      evaluated = ReportCardService.evaluateDeltas(cards.toList, patterns, existing.toList)
      _ <- if (persistence) abstractDao.createOrUpdateMany(evaluated) else Future.successful(Seq.empty)

      atomic = extractAttributes(request.queryString, defaultAtomic = false)._2.atomic
      evaluations <- if (atomic) abstractDao.getMany(evaluated.map(_.id), atomic) else Future.successful(evaluated.map(_.toLwmModel))
    } yield evaluations).jsonResult
  }

  override protected implicit val writes: Writes[ReportCardEvaluation] = ReportCardEvaluation.writes

  override protected implicit val reads: Reads[ReportCardEvaluationPattern] = (
    (JsPath \ "entryType").read[String] and
      (JsPath \ "min").read[Int] and
      (JsPath \ "property").read[String].map(p => if (p == "bool") BoolBased else IntBased)
    ) (ReportCardEvaluationPattern.apply _)


  override protected def tableFilter(attribute: String, value: String)(appendTo: Try[List[TableFilter[ReportCardEvaluationTable]]]): Try[List[TableFilter[ReportCardEvaluationTable]]] = {
    (appendTo, (attribute, value)) match {
      case (list, (`courseAttribute`, course)) => list.map(_.+:(CourseFilter(course)))
      case (list, (`labworkAttribute`, labwork)) => list.map(_.+:(LabworkFilter(labwork)))
      case (list, (`studentAttribute`, student)) => list.map(_.+:(StudentFilter(student)))
      case (list, (`labelAttribute`, label)) => list.map(_.+:(LabelFilter(label)))
      case (list, (`boolAttribute`, bool)) => list.map(_.+:(BoolFilter(bool)))
      case (list, (`intAttribute`, int)) => list.map(_.+:(IntFilter(int)))
      case (list, (`minIntAttribute`, minInt)) => list.map(_.+:(MinIntFilter(minInt)))
      case (list, (`maxIntAttribute`, maxInt)) => list.map(_.+:(MaxIntFilter(maxInt)))
      case (list, (`explicitAttribute`, explicit)) if explicit == true.toString => list.map(_.+:(IntFilter(ReportCardService.EvaluatedExplicit.toString)))
      case (_, (`explicitAttribute`, other)) => Failure(new Throwable(s"Value of $explicitAttribute can only be true, but was $other"))
      case _ => Failure(new Throwable("Unknown attribute"))
    }
  }

  override protected def toDbModel(protocol: ReportCardEvaluationPattern, existingId: Option[UUID]): ReportCardEvaluationDb = ???

  override implicit val mimeType: LwmMimeType = LwmMimeType.reportCardEvaluationV1Json

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case Create => SecureBlock(restrictionId, List(CourseManager))
    case Get => SecureBlock(restrictionId, List(CourseManager))
    case Delete => SecureBlock(restrictionId, List(CourseManager))
    case GetAll => SecureBlock(restrictionId, List(CourseManager, CourseEmployee))
    case _ => PartialSecureBlock(List(God))
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(List(Student))
    case _ => PartialSecureBlock(List(God))
  }
}
