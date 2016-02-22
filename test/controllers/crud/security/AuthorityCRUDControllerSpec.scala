package controllers.crud.security

import java.util.UUID

import controllers.crud.{AbstractCRUDController, AbstractCRUDControllerSpec}
import models.Course
import models.security._
import models.users.User
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{JsValue, Json, Writes}
import utils.LwmMimeType

class AuthorityCRUDControllerSpec extends AbstractCRUDControllerSpec[AuthorityProtocol, Authority] {

  override def entityTypeName: String = "authority"

  override val controller: AbstractCRUDController[AuthorityProtocol, Authority] = new AuthorityCRUDController(repository, namespace, roleService) {
    override protected def fromInput(input: AuthorityProtocol, id: Option[UUID]): Authority = entityToPass

    override protected def duplicate(input: AuthorityProtocol, output: Authority): Boolean = true
  }

  override val entityToFail: Authority = Authority(
    User.randomUUID,
    Set(RefRole(Some(Course.randomUUID), Role.randomUUID, RefRole.randomUUID)),
    Authority.randomUUID
  )

  override val entityToPass: Authority = Authority(
    User.randomUUID,
    Set(RefRole(Some(Course.randomUUID), Role.randomUUID, RefRole.randomUUID)),
    Authority.randomUUID
  )

  import ops._
  import bindings.AuthorityBinding._
  override val pointedGraph: PointedGraph[Sesame] = entityToPass.toPG

  override implicit val jsonWrites: Writes[Authority] = Authority.writes

  override val mimeType: LwmMimeType = LwmMimeType.authorityV1Json

  override val inputJson: JsValue = Json.obj(
    "user" -> entityToPass.user,
    "refRoles" -> entityToPass.refRoles
  )
}
