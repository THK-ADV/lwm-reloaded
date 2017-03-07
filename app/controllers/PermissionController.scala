package controllers

import models.Permissions
import models.SesamePermission
import modules.BaseNamespace
import play.api.libs.json.{Json, Reads, Writes}
import play.api.mvc.{Action, Controller}
import services.{RoleService, SessionHandlingService}
import store.{Namespace, SesameRepository}
import utils.LwmMimeType

class PermissionController(val repository: SesameRepository, val sessionService: SessionHandlingService, val namespace: Namespace, val roleService: RoleService) extends Controller
  with JsonSerialisation[SesamePermission, SesamePermission, SesamePermission]
  with BaseNamespace
  with Secured
  with SessionChecking
  with SecureControllerContext
  with ContentTyped {

  override implicit val mimeType: LwmMimeType = LwmMimeType.permissionV1Json

  override implicit val reads: Reads[SesamePermission] = SesamePermission.reads

  override implicit val writes: Writes[SesamePermission] = SesamePermission.writes

  override val writesAtom: Writes[SesamePermission] = SesamePermission.writesAtom

  def all(secureContext: SecureContext = contextFrom(GetAll)) = secureContext action { implicit request =>
    Ok(Json.toJson(Permissions.all)).as(mimeType)
  }

  def header = Action { implicit request =>
    NoContent.as(mimeType)
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case GetAll => PartialSecureBlock(Permissions.prime)
    case _ => PartialSecureBlock(Permissions.god)
  }
}
