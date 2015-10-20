package controllers.crud

import models.security.{Permission, Role, RoleProtocol}
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{JsValue, Json, Writes}
import play.api.mvc.{Action, Result, AnyContent, Request}
import utils.LWMActions.ContentTypedAction
import utils.LwmMimeType

class RoleCRUDControllerSpec extends AbstractCRUDControllerSpec[RoleProtocol, Role] {
  import bindings.RoleBinding._
  import ops._

  override def entityTypeName: String = "role"

  override val controller: AbstractCRUDController[RoleProtocol, Role] = new RoleCRUDController(repository, namespace, roleService) {

    override protected def invokeAction(rule: Rule)(moduleId: Option[String]): Block = new Block((None, Set())) {
      override def secured(block: (Request[AnyContent]) => Result): Action[AnyContent] = Action(block)
      override def secureContentTyped(block: (Request[JsValue]) => Result): Action[JsValue] = ContentTypedAction(block)(mimeType)
    }
  }

  override val entityToFail: Role = Role("role to fail", Set(Permission("permission to fail")), Role.randomUUID)

  override val entityToPass: Role = Role("role to pass", Set(Permission("permission to pass")), Role.randomUUID)

  override val pointedGraph: PointedGraph[Sesame] = entityToPass.toPG

  override implicit val jsonWrites: Writes[Role] = Role.writes

  override val mimeType: LwmMimeType = LwmMimeType.roleV1Json

  override val inputJson: JsValue = Json.obj(
    "name" -> "role",
    "permissions" -> Json.arr(Json.obj(
      "value" -> "perm"
    ))
  )
}
