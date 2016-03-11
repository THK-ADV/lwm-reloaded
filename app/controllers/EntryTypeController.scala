package controllers

import controllers.crud.{SecureControllerContext, Secured, ContentTyped}
import models.AssignmentEntryType
import models.security.Permissions
import modules.store.BaseNamespace
import play.api.libs.json.Json
import play.api.mvc.{Action, Controller}
import services.RoleService
import store.{Namespace, SesameRepository}
import utils.LwmMimeType
import utils.LwmMimeType._
import models.security.Permissions._

class EntryTypeController(val repository: SesameRepository, val namespace: Namespace, val roleService: RoleService) extends Controller
with ContentTyped
with BaseNamespace
with Secured
with SecureControllerContext {

  override implicit val mimeType: LwmMimeType = entryTypeV1Json

  def all(secureContext: SecureContext = contextFrom(GetAll)) = secureContext action { implicit request =>
    import AssignmentEntryType.protocolWrites

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
