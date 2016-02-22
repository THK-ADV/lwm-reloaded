package controllers

import controllers.crud.{JsonSerialisation, SecureControllerContext, Secured, ContentTyped}
import models.{EntryType, EntryTypes}
import models.security.Permissions
import modules.store.BaseNamespace
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
with SecureControllerContext {
  override implicit def reads: Reads[EntryType] = EntryType.reads

  override implicit def writes: Writes[EntryType] = EntryType.writes

  override implicit val mimeType: LwmMimeType = entryTypeV1Json

  def all(secureContext: SecureContext = contextFrom(All)) = secureContext action { implicit request =>
    Ok(Json.toJson(EntryTypes.types)).as(mimeType)
  }

  def header() = Action { implicit request =>
    NoContent.as(mimeType)
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case _ => PartialSecureBlock(Set(Permissions.prime))
  }
}
