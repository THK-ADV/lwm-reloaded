package controllers

import controllers.crud.{SecureControllerContext, Secured, ContentTyped, JsonSerialisation}
import models.security.Permissions._
import models.security.Roles._
import models.security.{Permissions, Permission}
import modules.BaseNamespace
import play.api.libs.json.{Json, Reads, Writes}
import play.api.mvc.{Action, Controller}
import services.RoleService
import store.{SesameRepository, Namespace}
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

  def all(secureContext: SecureContext = contextFrom(All)) = secureContext action { implicit request =>
    Ok(Json.toJson(Permissions.allValues)).as(mimeType)
  }

  def header = Action { implicit request =>
    NoContent.as(mimeType)
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case _ => PartialSecureBlock(Set(Permissions.prime))
  }
}
