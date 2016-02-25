package controllers.crud.security

import java.util.UUID

import controllers.crud.AbstractCRUDController
import models.UriGenerator
import models.security.{Authority, AuthorityProtocol}
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Json, JsValue, Reads, Writes}
import play.api.mvc.Result
import services.RoleService
import store.{Namespace, SesameRepository}
import utils.LwmMimeType

import scala.collection.Map
import scala.util.{Success, Try}

class AuthorityCRUDController(val repository: SesameRepository, val namespace: Namespace, val roleService: RoleService) extends AbstractCRUDController[AuthorityProtocol, Authority] {

  override implicit def rdfReads: FromPG[Sesame, Authority] = defaultBindings.AuthorityBinding.authorityBinder

  override implicit def classUrisFor: ClassUrisFor[Sesame, Authority] = defaultBindings.AuthorityBinding.classUri

  override implicit def uriGenerator: UriGenerator[Authority] = Authority

  override implicit def rdfWrites: ToPG[Sesame, Authority] = defaultBindings.AuthorityBinding.authorityBinder

  override implicit def reads: Reads[AuthorityProtocol] = Authority.reads

  override implicit def writes: Writes[Authority] = Authority.writes

  override protected def fromInput(input: AuthorityProtocol, id: Option[UUID]): Authority = id match {
    case Some(uuid) => Authority(input.user, input.refRoles, uuid)
    case None => Authority(input.user, input.refRoles, Authority.randomUUID)
  }

  override implicit val mimeType: LwmMimeType = LwmMimeType.authorityV1Json

  override protected def compareModel(input: AuthorityProtocol, output: Authority): Boolean = {
    input.user == output.user && input.refRoles == output.refRoles
  }

  override protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[Authority]): Try[Set[Authority]] = Success(all)

  override protected def atomize(output: Authority): Try[Option[JsValue]] = Success(Some(Json.toJson(output)))

  override protected def atomizeMany(output: Set[Authority]): Try[JsValue] = Success(Json.toJson(output))
}
