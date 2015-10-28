package controllers

import controllers.crud.{Deferred, Secured, ContentTyped, JsonSerialisation}
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
with Deferred
with ContentTyped  {
  override implicit def reads: Reads[Permission] = Permission.reads

  override implicit def writes: Writes[Permission] = Permission.writes

  override implicit val mimeType: LwmMimeType = LwmMimeType.permissionV1Json

  def all = invokeAction(All)(None) secured { implicit request =>
    Ok(Json.toJson(Permissions.allValues)).as(mimeType)
  }

  def header = Action { implicit request =>
    NoContent.as(mimeType)
  }

  override protected def invokeAction(act: Rule)(moduleId: Option[String] = None) = Invoke {
    case _ => Block((None, Set(Permissions.prime)))
  }.run(act)
}
