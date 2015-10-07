package controllers

import controllers.crud.{JsonSerialisation, Deferred, Secured, ContentTyped}
import models.{EntryType, EntryTypes}
import models.security.Permissions
import modules.BaseNamespace
import play.api.libs.json.{Reads, Writes, Json}
import play.api.mvc.{Action, Controller}
import services.RoleService
import store.{Namespace, SesameRepository}
import utils.LwmMimeType
import utils.LwmMimeType._

class EntryTypeController(val repository: SesameRepository, val namespace: Namespace, val roleService: RoleService) extends Controller
with JsonSerialisation[EntryType, EntryType]
with ContentTyped
with BaseNamespace
with Secured
with Deferred {
  override implicit def reads: Reads[EntryType] = EntryType.reads

  override implicit def writes: Writes[EntryType] = EntryType.writes

  override implicit val mimeType: LwmMimeType = entryTypeV1Json

  def all() = invokeAction(All)(None) secured { implicit request =>
    Ok(Json.toJson(EntryTypes.types)).as(mimeType)
  }

  def header() = Action { implicit request =>
    NoContent.as(mimeType)
  }

  override protected def invokeAction(rule: Rule)(moduleId: Option[String]): Block = Invoke {
    case _ => Block((None, Set(Permissions.prime)))
  }.run(rule)

}
