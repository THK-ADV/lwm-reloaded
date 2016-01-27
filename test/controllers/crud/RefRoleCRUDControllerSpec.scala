package controllers.crud

import java.util.UUID

import models.Course
import models.security.{Permission, RefRole, RefRoleProtocol, Role}
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.http.HeaderNames
import play.api.libs.json.{JsValue, Json, Writes}
import play.api.mvc.{Action, Result, AnyContent, Request}
import play.api.test.Helpers._
import play.api.test.{FakeHeaders, FakeRequest}
import utils.LWMActions.ContentTypedAction
import utils.LwmMimeType

import scala.util.Success

class RefRoleCRUDControllerSpec extends AbstractCRUDControllerSpec[RefRoleProtocol, RefRole] {

  import bindings.RefRoleBinding._
  import ops._

  override def entityTypeName: String = "refRole"

  override val controller: AbstractCRUDController[RefRoleProtocol, RefRole] = new RefRoleCRUDController(repository, namespace, roleService) {
    override protected def fromInput(input: RefRoleProtocol, id: Option[UUID]): RefRole = entityToPass

    override protected def duplicate(input: RefRoleProtocol, output: RefRole): Boolean = true
  }

  override val entityToFail: RefRole = RefRole(Some(Course.randomUUID), Role.randomUUID, RefRole.randomUUID)

  override val entityToPass: RefRole = RefRole(Some(Course.randomUUID), Role.randomUUID, RefRole.randomUUID)

  override implicit val jsonWrites: Writes[RefRole] = RefRole.writes

  override val mimeType: LwmMimeType = LwmMimeType.refRoleV1Json

  override val pointedGraph: PointedGraph[Sesame] = entityToPass.toPG

  override val inputJson: JsValue = Json.obj(
    "module" -> entityToPass.module,
    "role" -> entityToPass.role
  )
}
