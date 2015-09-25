package controllers.crud

import models.Course
import models.security._
import models.users.User
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{JsValue, Json, Writes}
import play.api.mvc.{Action, Result, AnyContent, Request}
import utils.LWMActions.ContentTypedAction
import utils.LwmMimeType

class AuthorityCRUDControllerSpec extends AbstractCRUDControllerSpec[AuthorityProtocol, Authority] {

  override def entityTypeName: String = "authority"

  override val controller: AbstractCRUDController[AuthorityProtocol, Authority] = new AuthorityCRUDController(repository, namespace, roleService) {

    override protected def invokeAction(act: Rule)(moduleId: Option[String]): Block = new Block((None, Set())) {
      override def secured(block: (Request[AnyContent]) => Result): Action[AnyContent] = Action(block)
      override def securedt(block: (Request[JsValue]) => Result): Action[JsValue] = ContentTypedAction(block)(mimeType)
    }

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

  import bindings.AuthorityBinding._
  import ops._

  override val pointedGraph: PointedGraph[Sesame] = entityToPass.toPG

  override implicit val jsonWrites: Writes[Authority] = Authority.writes

  override val mimeType: LwmMimeType = LwmMimeType.authorityV1Json

  override val inputJson: JsValue = Json.obj(
    "user" -> User.randomUUID,
    "refRoles" -> Json.arr(Json.obj(
      "module" -> Some(Course.randomUUID.toString),
      "role" -> Role.randomUUID.toString,
      "id" -> RefRole.randomUUID.toString
    ))
  )
}
