package controllers.security

import controllers.crud.{ContentTyped, JsonSerialisation, SecureControllerContext, Secured}
import models.security.{Permission, Permissions}
import modules.store.BaseNamespace
import play.api.libs.json.{Json, Reads, Writes}
import play.api.mvc.{Action, Controller}
import services.RoleService
import store.{Namespace, SesameRepository}
import utils.LwmMimeType

class PermissionController(val repository: SesameRepository, val namespace: Namespace, val roleService: RoleService) extends Controller
  with JsonSerialisation[Permission, Permission]
  with BaseNamespace
  with Secured
  with SecureControllerContext
  with ContentTyped {

  override implicit val mimeType: LwmMimeType = LwmMimeType.permissionV1Json

  override implicit def reads: Reads[Permission] = Permission.reads

  override implicit def writes: Writes[Permission] = Permission.writes

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
