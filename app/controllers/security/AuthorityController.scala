package controllers.security

import java.util.UUID

import controllers.crud._
import models.security.Permissions._
import models.security._
import models.users.{Employee, User}
import models.{Course, CourseAtom, UriGenerator}
import org.openrdf.model.Value
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json._
import play.api.mvc.{Action, AnyContent}
import services.{RoleService, SessionHandlingService}
import store.Prefixes.LWMPrefix
import store.bind.Descriptor.{CompositeClassUris, Descriptor}
import store.sparql.Clause
import store.{Namespace, SesameRepository}
import utils.LwmMimeType
import utils.Ops.MonadInstances.{listM, optM, tryM}
import utils.Ops.TraverseInstances._
import utils.Ops._

import scala.collection.Map
import scala.util.{Failure, Success, Try}

object AuthorityController {
  val userAttribute = "user"
  val courseAttribute = "course"
  val roleAttribute = "role"
}

class AuthorityController(val repository: SesameRepository, val sessionService: SessionHandlingService, val namespace: Namespace, val roleService: RoleService) extends AbstractCRUDController[AuthorityProtocol, Authority] {

  override implicit def uriGenerator: UriGenerator[Authority] = Authority

  override implicit def descriptor: Descriptor[Sesame, Authority] = defaultBindings.AuthorityDescriptor

  override implicit def reads: Reads[AuthorityProtocol] = Authority.reads

  override implicit def writes: Writes[Authority] = Authority.writes

  override protected def fromInput(input: AuthorityProtocol, existing: Option[Authority]): Authority = existing match {
    case Some(authority) => Authority(input.user, input.refRoles, authority.id)
    case None => Authority(input.user, input.refRoles, Authority.randomUUID)
  }

  override implicit val mimeType: LwmMimeType = LwmMimeType.authorityV1Json

  override protected def atomize(output: Authority): Try[Option[JsValue]] = {
    import utils.Ops._
    import defaultBindings.AuthorityAtomDescriptor
    import Authority.writesAtomic

    implicit val ns = repository.namespace
    repository.get[AuthorityAtom](Authority.generateUri(output)).peek (Json.toJson(_)) (tryM, optM)
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Update => PartialSecureBlock(authority.update)
    case GetAll => PartialSecureBlock(authority.getAll)
    case Get => PartialSecureBlock(authority.get)
    case _ => PartialSecureBlock(god)
  }

  // TODO allAtomic should chunk responses by default
  override def allAtomic(securedContext: SecureContext = contextFrom(GetAll)): Action[AnyContent] = securedContext action { request =>
    val res = {
      if (request.queryString.nonEmpty)
        getWithFilter(request.queryString)(Set.empty) map (set => chunkAtoms(set))
      else {
        repository.getAll[Authority] map (set => chunkAtoms(set))
      }
    }

    res match {
      case Success(enum) =>
        Ok.chunked(enum).as(mimeType)
      case Failure(e) =>
        InternalServerError(Json.obj(
          "status" -> "KO",
          "errors" -> e.getMessage
        ))
    }
  }

  override protected def compareModel(input: AuthorityProtocol, output: Authority): Boolean = input.refRoles == output.refRoles

  override protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[Authority]): Try[Set[Authority]] = {
    import AuthorityController._
    import store.sparql.select
    import store.sparql.select._

    val lwm = LWMPrefix[repository.Rdf]
    implicit val ns = repository.namespace
    val clause: Clause = **(v("auth"), p(lwm.refroles), v("refs"))

    queryString.foldRight(Try(clause)) {
      case ((`courseAttribute`, set), t) => t map {
        _ append **(v("refs"), p(lwm.course), s(Course.generateUri(UUID.fromString(set.head))))
      }
      case ((`roleAttribute`, set), t) => t map {
        _ append **(v("refs"), p(lwm.role), s(Role.generateUri(UUID.fromString(set.head))))
      }
      case ((`userAttribute`, set), t) => t map {
        _ append **(v("auth"), p(lwm.privileged), s(User.generateUri(UUID.fromString(set.head))))
      }
      case _ => Failure(new Throwable("Unknown attribute"))
    } flatMap { clause =>
      val query = select distinct "auth" where clause

      repository.prepareQuery(query).
        select(_.get("auth")).
        transform(_.fold(List.empty[Value])(identity)).
        map(_.stringValue()).
        requestAll(repository.getMany[Authority](_)).
        run
    }
  }
}
