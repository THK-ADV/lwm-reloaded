package controllers.reportCard

import controllers.crud._
import models.UriGenerator
import models.labwork.ReportCardEntry
import modules.store.BaseNamespace
import org.w3.banana.binder.{FromPG, ClassUrisFor, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json._
import play.api.mvc.{Action, Controller}
import services.{RoleService, SessionHandlingService}
import store.{Namespace, SesameRepository}
import utils.LwmMimeType
import models.security.Permissions._

import scala.util.{Failure, Success}

class ReportCardEntryController(val repository: SesameRepository, val sessionService: SessionHandlingService, val namespace: Namespace, val roleService: RoleService) extends Controller
  with BaseNamespace
  with JsonSerialisation[ReportCardEntry, ReportCardEntry]
  with SesameRdfSerialisation[ReportCardEntry]
  with ContentTyped
  with Secured
  with SessionChecking
  with SecureControllerContext {

  override implicit def reads: Reads[ReportCardEntry] = ReportCardEntry.reads

  override implicit def writes: Writes[ReportCardEntry] = ReportCardEntry.writes

  override implicit def rdfReads: FromPG[Sesame, ReportCardEntry] = defaultBindings.ReportCardEntryBinding.reportCardEntryBinding

  override implicit def classUrisFor: ClassUrisFor[Sesame, ReportCardEntry] = defaultBindings.ReportCardEntryBinding.classUri

  override implicit def uriGenerator: UriGenerator[ReportCardEntry] = ReportCardEntry

  override implicit def rdfWrites: ToPG[Sesame, ReportCardEntry] = defaultBindings.ReportCardEntryBinding.reportCardEntryBinding

  override implicit val mimeType: LwmMimeType = LwmMimeType.reportCardEntryV1Json

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case _ => PartialSecureBlock(god)
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case Update => SecureBlock(restrictionId, reportCardEntry.update)
    case _ => PartialSecureBlock(god)
  }

  def update(course: String, card: String, entry: String) = restrictedContext(course)(Update) contentTypedAction { request =>
    request.body.validate[ReportCardEntry].fold(
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