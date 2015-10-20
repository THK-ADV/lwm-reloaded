package controllers.crud

import java.util.UUID

import models.UriGenerator
import models.security.{Role, RoleProtocol}
import org.w3.banana.binder.{FromPG, ClassUrisFor, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.Result
import services.RoleService
import store.{Namespace, SesameRepository}
import utils.LwmMimeType

import scala.collection.Map

class RoleCRUDController(val repository: SesameRepository, val namespace: Namespace, val roleService: RoleService) extends AbstractCRUDController[RoleProtocol, Role] {

  override implicit def reads: Reads[RoleProtocol] = Role.reads

  override implicit def writes: Writes[Role] = Role.writes

  override implicit def rdfReads: FromPG[Sesame, Role] = defaultBindings.RoleBinding.roleBinder

  override implicit def classUrisFor: ClassUrisFor[Sesame, Role] = defaultBindings.RoleBinding.classUri

  override implicit def uriGenerator: UriGenerator[Role] = Role

  override implicit def rdfWrites: ToPG[Sesame, Role] = defaultBindings.RoleBinding.roleBinder

  override implicit val mimeType: LwmMimeType = LwmMimeType.roleV1Json

  override protected def fromInput(input: RoleProtocol, id: Option[UUID]): Role = id match {
    case Some(x) => Role(input.name, input.permissions, x)
    case None => Role(input.name, input.permissions)
  }

  override def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[Role]): Result = ???
}
