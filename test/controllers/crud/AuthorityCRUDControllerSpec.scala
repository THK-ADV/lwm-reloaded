package controllers.crud

import models.Course
import models.security._
import models.users.User
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{JsValue, Json, Writes}
import utils.LwmMimeType
//TODO: BUGGY. REPAIR. NAOW!
class AuthorityCRUDControllerSpec extends AbstractCRUDControllerSpec[AuthorityProtocol, Authority] {

  override def entityTypeName: String = "authority"

  override val controller: AbstractCRUDController[AuthorityProtocol, Authority] = new AuthorityCRUDController(repository, namespace, roleService)

  override val entityToFail: Authority = Authority(
    User.randomUUID,
    Set(RefRole(Some(Course.randomUUID), Role("role to fail", Set(Permission("perm to fail"))), RefRole.randomUUID)),
    Authority.randomUUID
  )

  override val entityToPass: Authority = Authority(
    User.randomUUID,
    Set(RefRole(Some(Course.randomUUID), Role("role to pass", Set(Permission("perm to pass"))), RefRole.randomUUID)),
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
      "role" -> Json.obj(
        "name" -> "role input",
        "permissions" -> Json.arr(Json.obj(
          "value" -> "perm"
        ))
      ),
      "id" -> RefRole.randomUUID.toString
    ))
  )
}
