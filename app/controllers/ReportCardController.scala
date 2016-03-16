package controllers

import controllers.crud._
import models.{ReportCardEntryType, UriGenerator, ReportCard}
import modules.store.BaseNamespace
import org.w3.banana.binder.{FromPG, ClassUrisFor, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json._
import services.{RoleService, SessionHandlingService}
import store.{Namespace, SesameRepository}
import utils.LwmMimeType
import play.api.mvc._
import models.security.Permissions._
import scala.util.{Failure, Success, Try}

class ReportCardController(val repository: SesameRepository, val sessionService: SessionHandlingService, val namespace: Namespace, val roleService: RoleService) extends Controller
  with BaseNamespace
  with JsonSerialisation[ReportCard, ReportCard]
  with SesameRdfSerialisation[ReportCard]
  with Atomic[ReportCard]
  with ContentTyped
  with Secured
  with SessionChecking
  with SecureControllerContext {

  override implicit def rdfReads: FromPG[Sesame, ReportCard] = defaultBindings.ReportCardBinding.reportCardBinder

  override implicit def classUrisFor: ClassUrisFor[Sesame, ReportCard] = defaultBindings.ReportCardBinding.classUri

  override implicit def uriGenerator: UriGenerator[ReportCard] = ReportCard

  override implicit def rdfWrites: ToPG[Sesame, ReportCard] = defaultBindings.ReportCardBinding.reportCardBinder

  override implicit val mimeType: LwmMimeType = LwmMimeType.reportCardV1Json

  override protected def atomize(output: ReportCard): Try[Option[JsValue]] = Success(Some(Json.toJson(output)))

  override implicit def reads: Reads[ReportCard] = ReportCard.reads

  override implicit def writes: Writes[ReportCard] = ReportCard.writes

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case _ => PartialSecureBlock(god)
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case Update => SecureBlock(restrictionId, reportCard.update)
    case _ => PartialSecureBlock(god)
  }

  def updateReportCardEntryType(course: String, card: String, cardEntry: String) = restrictedContext(course)(Update) contentTypedAction { request =>
    import ReportCardEntryType._
    import defaultBindings.ReportCardEntryTypeBinding.reportCardEntryTypeBinding

    request.body.validate[ReportCardEntryType].fold(
      errors => {
        BadRequest(Json.obj(
          "status" -> "KO",
          "errors" -> JsError.toJson(errors)
        ))
      },
      success => {
        repository.update(success)(reportCardEntryTypeBinding, ReportCardEntryType) match {
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
}
