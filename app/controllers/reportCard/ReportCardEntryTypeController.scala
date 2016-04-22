package controllers.reportCard

import java.util.UUID

import controllers.crud._
import models.UriGenerator
import models.labwork.ReportCardEntryType
import models.security.Permissions.{god, reportCardEntryType}
import modules.store.BaseNamespace
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{JsError, Json, Reads, Writes}
import play.api.mvc.{Action, Controller}
import services.{RoleService, SessionHandlingService}
import store.{Namespace, SesameRepository}
import utils.LwmMimeType

import scala.util.{Failure, Success}

class ReportCardEntryTypeController(val repository: SesameRepository, val sessionService: SessionHandlingService, implicit val namespace: Namespace, val roleService: RoleService)
  extends Controller
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

  def update(course: String, entryType: String) = restrictedContext(course)(Update) contentTypedAction { request =>
    request.body.validate[ReportCardEntryType].fold(
      errors => {
        BadRequest(Json.obj(
          "status" -> "KO",
          "errors" -> JsError.toJson(errors)
        ))
      },
      success => {
        if (success.id == UUID.fromString(entryType))
          repository.update(success) match {
            case Success(_) =>
              Ok(Json.toJson(success)).as(mimeType)
            case Failure(e) =>
              InternalServerError(Json.obj(
                "status" -> "KO",
                "errors" -> e.getMessage
              ))
          }
        else
          BadRequest(Json.obj(
            "status" -> "KO",
            "message" -> s"Id found in body (${success.id}) does not match id found in resource ($entryType)"
          ))
      }
    )
  }

  def header = Action { implicit request =>
    NoContent.as(mimeType)
  }
}
