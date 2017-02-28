package controllers

import models._
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{JsValue, Json, Writes}
import utils.LwmMimeType

class RoleControllerSpec extends AbstractCRUDControllerSpec[SesameRoleProtocol, SesameRole, SesameRole] {
  import bindings.RoleDescriptor
  import ops._

  override def entityTypeName: String = "role"

  override val controller: RoleController = new RoleController(repository, sessionService, namespace, roleService) {

    override protected def fromInput(input: SesameRoleProtocol, existing: Option[SesameRole]): SesameRole = entityToPass

    override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }
  }
  override val entityToFail: SesameRole = SesameRole("role to fail", Set(Permission("permission to fail")))

  override val entityToPass: SesameRole = SesameRole("role to pass", Set(Permission("permission to pass")))

  override implicit val jsonWrites: Writes[SesameRole] = SesameRole.writes

  override val atomizedEntityToPass: SesameRole = entityToPass

  override val atomizedEntityToFail: SesameRole = entityToFail

  override val jsonWritesAtom: Writes[SesameRole] = jsonWrites

  implicit val roleBinder = RoleDescriptor.binder

  override val pointedGraph: PointedGraph[Sesame] = entityToPass.toPG

  override val mimeType: LwmMimeType = LwmMimeType.roleV1Json

  override val inputJson: JsValue = Json.obj(
    "label" -> entityToPass.label,
    "permissions" -> entityToPass.permissions
  )

  override val updateJson: JsValue = Json.obj(
    "label" -> entityToPass.label,
    "permissions" -> (entityToPass.permissions + Permission(""))
  )
}
