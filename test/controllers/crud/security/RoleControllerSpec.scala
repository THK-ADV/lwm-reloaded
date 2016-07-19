package controllers.crud.security

import controllers.crud.AbstractCRUDControllerSpec
import controllers.security.RoleController
import models.security.{Permission, Role, RoleProtocol}
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{JsValue, Json, Writes}
import utils.LwmMimeType

class RoleControllerSpec extends AbstractCRUDControllerSpec[RoleProtocol, Role, Role] {
  import ops._
  import bindings.RoleDescriptor
  override def entityTypeName: String = "role"

  override val controller: RoleController = new RoleController(repository, sessionService, namespace, roleService) {

    override protected def fromInput(input: RoleProtocol, existing: Option[Role]): Role = entityToPass

    override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }
  }
  override val entityToFail: Role = Role("role to fail", Set(Permission("permission to fail")))

  override val entityToPass: Role = Role("role to pass", Set(Permission("permission to pass")))

  override implicit val jsonWrites: Writes[Role] = Role.writes

  override val atomizedEntityToPass: Role = entityToPass

  override val atomizedEntityToFail: Role = entityToFail

  override val jsonWritesAtom: Writes[Role] = jsonWrites

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
