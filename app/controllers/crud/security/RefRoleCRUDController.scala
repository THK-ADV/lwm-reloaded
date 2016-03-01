package controllers.crud.security

import java.util.UUID

import controllers.crud.AbstractCRUDController
import models.UriGenerator
import models.security.{RefRole, RefRoleProtocol}
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Json, JsValue, Reads, Writes}
import play.api.mvc.Result
import services.RoleService
import store.{Namespace, SesameRepository}
import utils.LwmMimeType

import scala.collection.Map
import scala.util.{Success, Try}

class RefRoleCRUDController(val repository: SesameRepository, val namespace: Namespace, val roleService: RoleService) extends AbstractCRUDController[RefRoleProtocol, RefRole]{
  override implicit def reads: Reads[RefRoleProtocol] = RefRole.reads

  override implicit def writes: Writes[RefRole] = RefRole.writes

  override implicit def rdfReads: FromPG[Sesame, RefRole] = defaultBindings.RefRoleBinding.refRoleBinder

  override implicit def classUrisFor: ClassUrisFor[Sesame, RefRole] = defaultBindings.RefRoleBinding.classUri

  override implicit def uriGenerator: UriGenerator[RefRole] = RefRole

  override implicit def rdfWrites: ToPG[Sesame, RefRole] = defaultBindings.RefRoleBinding.refRoleBinder

  override protected def fromInput(input: RefRoleProtocol, id: Option[UUID]): RefRole = id match {
    case Some(uuid) => RefRole(input.module, input.role, uuid)
    case None => RefRole(input.module, input.role, RefRole.randomUUID)
  }

  override implicit val mimeType: LwmMimeType = LwmMimeType.refRoleV1Json

  override protected def compareModel(input: RefRoleProtocol, output: RefRole): Boolean = {
    input.role == output.role && input.module == output.module
  }

  override protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[RefRole]): Try[Set[RefRole]] = Success(all)

  override protected def atomize(output: RefRole): Try[Option[JsValue]] = Success(Some(Json.toJson(output)))

  override protected def atomizeMany(output: Set[RefRole]): Try[JsValue] = Success(Json.toJson(output))
}
