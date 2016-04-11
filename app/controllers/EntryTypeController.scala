package controllers

import controllers.crud.{ContentTyped, SecureControllerContext, Secured, SessionChecking}
import models.labwork.AssignmentEntryType
import models.security.Permissions
import modules.store.BaseNamespace
import play.api.libs.json.Json
import play.api.mvc.{Action, Controller}
import services.{RoleService, SessionHandlingService}
import store.{Namespace, SesameRepository}
import utils.LwmMimeType
import utils.LwmMimeType._
import models.security.Permissions._

class EntryTypeController(val repository: SesameRepository, val sessionService: SessionHandlingService, val namespace: Namespace, val roleService: RoleService) extends Controller
with ContentTyped
with BaseNamespace
with Secured
with SessionChecking
with SecureControllerContext {

  override implicit val mimeType: LwmMimeType = entryTypeV1Json

  def all(secureContext: SecureContext = contextFrom(GetAll)) = secureContext action { implicit request =>
    Ok(Json.toJson(AssignmentEntryType.all)).as(mimeType)
  }

  def header() = Action { implicit request =>
    NoContent.as(mimeType)
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case GetAll => PartialSecureBlock(entryType.getAll)
    case _ => PartialSecureBlock(Permissions.god)
  }
}