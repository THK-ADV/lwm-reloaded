package controllers

import models.SesameAssignmentEntryType
import play.api.libs.json.{Json, Writes}
import play.api.mvc.{Action, Controller}
import services.{RoleServiceLike, SessionHandlingService}
import store.{Namespace, SesameRepository}
import utils.LwmMimeType
import utils.LwmMimeType._
import models.Permissions.{entryType, god}
import modules.BaseNamespace

class EntryTypeController(val repository: SesameRepository, val sessionService: SessionHandlingService, val namespace: Namespace, val roleService: RoleServiceLike) extends Controller
  with ContentTyped
  with BaseNamespace
  with Secured
  with SessionChecking
  with SecureControllerContext {

  override implicit val mimeType: LwmMimeType = entryTypeV1Json

  def all(secureContext: SecureContext = contextFrom(GetAll)) = secureContext action { implicit request =>
    Ok(Json.toJson(SesameAssignmentEntryType.all)(Writes.set(SesameAssignmentEntryType.writes))).as(mimeType)
  }

  def header() = Action { implicit request =>
    NoContent.as(mimeType)
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case GetAll => PartialSecureBlock(entryType.getAll)
    case _ => PartialSecureBlock(god)
  }
}