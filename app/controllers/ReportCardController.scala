package controllers

import java.util.UUID

import controllers.crud.AbstractCRUDController
import models.{AssignmentEntryType, ReportCard, UriGenerator}
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json._
import services.{RoleService, SessionHandlingService}
import store.{Namespace, SesameRepository}
import utils.LwmMimeType
import models.security.Permissions._

import scala.collection.Map
import scala.util.{Failure, Success, Try}

class ReportCardController(val repository: SesameRepository, val sessionService: SessionHandlingService, val namespace: Namespace, val roleService: RoleService) extends AbstractCRUDController[ReportCard, ReportCard] {

  override protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[ReportCard]): Try[Set[ReportCard]] = Success(all)

  override implicit def rdfReads: FromPG[Sesame, ReportCard] = defaultBindings.ReportCardBinding.reportCardBinder

  override implicit def classUrisFor: ClassUrisFor[Sesame, ReportCard] = defaultBindings.ReportCardBinding.classUri

  override implicit def uriGenerator: UriGenerator[ReportCard] = ReportCard

  override implicit def rdfWrites: ToPG[Sesame, ReportCard] = defaultBindings.ReportCardBinding.reportCardBinder

  override implicit val mimeType: LwmMimeType = LwmMimeType.reportCardV1Json

  override protected def atomize(output: ReportCard): Try[Option[JsValue]] = Success(Some(Json.toJson(output)))

  override implicit def writes: Writes[ReportCard] = ReportCard.writes

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(reportCard.get)
    case _ => PartialSecureBlock(god)
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case GetAll => SecureBlock(restrictionId, reportCard.getAll)
    case Get => SecureBlock(restrictionId, reportCard.get)
    case Update => SecureBlock(restrictionId, reportCard.update)
    case _ => PartialSecureBlock(god)
  }

  def updateReportCardEntry(course: String, card: String, cardEntry: String) = restrictedContext(course)(Update) contentTypedAction { request =>
    import AssignmentEntryType._
    import defaultBindings.AssignmentEntryTypeBinding.assignmentEntryTypeBinder

    request.body.validate[AssignmentEntryType].fold(
      errors => {
        BadRequest(Json.obj(
          "status" -> "KO",
          "errors" -> JsError.toJson(errors)
        ))
      },
      success => {
        repository.update(success)(assignmentEntryTypeBinder, AssignmentEntryType) match {
          case Success(s) =>
            Ok(Json.obj(
              "reportCardId" -> card,
              "reportCardEntryId" -> cardEntry,
              "assignmentEntryType" -> Json.toJson(success)
            ))
          case Failure(e) =>
            InternalServerError(Json.obj(
              "status" -> "KO",
              "errors" -> e.getMessage
            ))
        }
      }
    )
  }

  def getFrom(course: String, reportCard: String) = restrictedContext(course)(Get) asyncAction { request =>
    val newRequest = AbstractCRUDController.rebaseUri(request, ReportCard.generateBase(UUID.fromString(reportCard)))
    super.get(reportCard, NonSecureBlock)(newRequest)
  }

  def allFrom(course: String) = restrictedContext(course)(GetAll) asyncAction { request =>
    val newRequest = AbstractCRUDController.rebaseUri(request, ReportCard.generateBase)
    super.all(NonSecureBlock)(newRequest)
  }

  def updateFrom(course: String, reportCard: String) = restrictedContext(course)(Update) asyncContentTypedAction { request =>
    val newRequest = AbstractCRUDController.rebaseUri(request, ReportCard.generateBase(UUID.fromString(reportCard)))
    super.update(reportCard, NonSecureBlock)(newRequest)
  }

  override implicit def reads: Reads[ReportCard] = ReportCard.reads

  override protected def fromInput(input: ReportCard, existing: Option[ReportCard]): ReportCard = input

  override protected def compareModel(input: ReportCard, output: ReportCard): Boolean = input.entries == output.entries
}
