package controllers.crud.security

import java.util.UUID

import controllers.crud.{AbstractCRUDController, AbstractCRUDControllerSpec}
import models.security.{Permission, Role, RoleProtocol}
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{JsValue, Json, Writes}
import store.SesameRepository
import utils.LwmMimeType

class RoleCRUDControllerSpec extends AbstractCRUDControllerSpec[RoleProtocol, Role] {
  import ops._
  import bindings.RoleBinding._
  override def entityTypeName: String = "role"

  override val controller: AbstractCRUDController[RoleProtocol, Role] = new RoleCRUDController(repository, namespace, roleService) {

    override protected def fromInput(input: RoleProtocol, id: Option[UUID]): Role = entityToPass
  }

  override val entityToFail: Role = Role("role to fail", Set(Permission("permission to fail")), Role.randomUUID)

  override val entityToPass: Role = Role("role to pass", Set(Permission("permission to pass")), Role.randomUUID)

  override val pointedGraph: PointedGraph[Sesame] = entityToPass.toPG

  override implicit val jsonWrites: Writes[Role] = Role.writes

  override val mimeType: LwmMimeType = LwmMimeType.roleV1Json

  override val inputJson: JsValue = Json.obj(
    "name" -> entityToPass.name,
    "permissions" -> entityToPass.permissions
  )

  override val updateJson: JsValue = Json.obj(
    "name" -> entityToPass.name,
    "permissions" -> (entityToPass.permissions + Permission(""))
  )
}
