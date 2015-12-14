package controllers.crud.security

import java.util.UUID

import controllers.crud.{AbstractCRUDController, AbstractCRUDControllerSpec}
import models.Course
import models.security.{RefRole, RefRoleProtocol, Role}
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{JsValue, Json, Writes}
import utils.LwmMimeType

class RefRoleCRUDControllerSpec extends AbstractCRUDControllerSpec[RefRoleProtocol, RefRole] {

  import ops._
  import bindings.RefRoleBinding._
  override def entityTypeName: String = "refRole"

  override val controller: AbstractCRUDController[RefRoleProtocol, RefRole] = new RefRoleCRUDController(repository, namespace, roleService) {
    override protected def fromInput(input: RefRoleProtocol, id: Option[UUID]): RefRole = entityToPass
  }

  override val entityToFail: RefRole = RefRole(Some(Course.randomUUID), Role.randomUUID, RefRole.randomUUID)

  override val entityToPass: RefRole = RefRole(Some(Course.randomUUID), Role.randomUUID, RefRole.randomUUID)

  override implicit val jsonWrites: Writes[RefRole] = RefRole.writes

  override val mimeType: LwmMimeType = LwmMimeType.refRoleV1Json

  override val pointedGraph: PointedGraph[Sesame] = entityToPass.toPG

  override val inputJson: JsValue = Json.obj(
    "module" -> Some(Course.randomUUID.toString),
    "role" -> Role.randomUUID
  )
}
