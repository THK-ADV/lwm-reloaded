package controllers.reportCard

import controllers.crud._
import models.UriGenerator
import models.labwork.ReportCardEntryType
import modules.store.BaseNamespace
import org.w3.banana.binder.{FromPG, ClassUrisFor, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{JsError, Json, Reads, Writes}
import play.api.mvc.Controller
import services.{RoleService, SessionHandlingService}
import store.{Namespace, SesameRepository}
import utils.LwmMimeType
import models.security.Permissions._
import scala.util.{Failure, Success}

class ReportCardEntryTypeController(val repository: SesameRepository, val sessionService: SessionHandlingService, val namespace: Namespace, val roleService: RoleService) extends Controller
  with BaseNamespace
  with JsonSerialisation[ReportCardEntryType, ReportCardEntryType]
  with SesameRdfSerialisation[ReportCardEntryType]
  with ContentTyped
  with Secured
  with SessionChecking
  with SecureControllerContext {

  override implicit def reads: Reads[ReportCardEntryType] = ReportCardEntryType.reads

  override implicit def writes: Writes[ReportCardEntryType] = ReportCardEntryType.writes

  override implicit def rdfReads: FromPG[Sesame, ReportCardEntryType] = defaultBindings.ReportCardEntryTypeBinding.reportCardEntryTypeBinding

  override implicit def classUrisFor: ClassUrisFor[Sesame, ReportCardEntryType] = defaultBindings.ReportCardEntryTypeBinding.classUri

  override implicit def uriGenerator: UriGenerator[ReportCardEntryType] = ReportCardEntryType

  override implicit def rdfWrites: ToPG[Sesame, ReportCardEntryType] = defaultBindings.ReportCardEntryTypeBinding.reportCardEntryTypeBinding

  override implicit val mimeType: LwmMimeType = LwmMimeType.reportCardEntryTypeV1Json

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case _ => PartialSecureBlock(god)
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case Update => SecureBlock(restrictionId, reportCardEntryType.update)
    case _ => PartialSecureBlock(god)
  }

  def update(course: String, card: String, cardEntry: String, entryType: String) = restrictedContext(course)(Update) contentTypedAction { request =>
    request.body.validate[ReportCardEntryType].fold(
      errors => {
        BadRequest(Json.obj(
          "status" -> "KO",
          "errors" -> JsError.toJson(errors)
        ))
      },
      success => {
        repository.update(success) match {
          case Success(_) =>
            Ok(Json.toJson(success)).as(mimeType)
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
