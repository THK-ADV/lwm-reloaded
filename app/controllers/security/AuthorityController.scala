package controllers.security

import java.util.UUID

import controllers.crud._
import models.security.Permissions._
import models.security._
import models.users.User
import models.{Course, UriGenerator}
import org.openrdf.model.Value
import org.w3.banana.sesame.Sesame
import play.api.libs.json._
import play.api.mvc.{Action, AnyContent}
import services.{RoleService, SessionHandlingService}
import store.Prefixes.LWMPrefix
import store.bind.Descriptor.Descriptor
import store.sparql.Clause
import store.{Namespace, SesameRepository}
import utils.LwmMimeType
import utils.Ops.MonadInstances.listM

import scala.collection.Map
import scala.util.{Failure, Try}

object AuthorityController {
  val userAttribute = "user"
  val courseAttribute = "course"
  val roleAttribute = "role"
}

class AuthorityController(val repository: SesameRepository, val sessionService: SessionHandlingService, implicit val namespace: Namespace, val roleService: RoleService) extends AbstractCRUDController[AuthorityProtocol, Authority, AuthorityAtom] {

  override implicit val mimeType: LwmMimeType = LwmMimeType.authorityV1Json

  override implicit val descriptor: Descriptor[Sesame, Authority] = defaultBindings.AuthorityDescriptor

  override implicit val descriptorAtom: Descriptor[Sesame, AuthorityAtom] = defaultBindings.AuthorityAtomDescriptor

  override implicit val reads: Reads[AuthorityProtocol] = Authority.reads

  override implicit val writes: Writes[Authority] = Authority.writes

  override implicit val writesAtom: Writes[AuthorityAtom] = Authority.writesAtom

  override implicit val uriGenerator: UriGenerator[Authority] = Authority

  override protected def coatomic(atom: AuthorityAtom): Authority = Authority(atom.user.id, atom.refRoles map (_.id), atom.invalidated, atom.id)

  override protected def compareModel(input: AuthorityProtocol, output: Authority): Boolean = input.refRoles == output.refRoles

  override protected def fromInput(input: AuthorityProtocol, existing: Option[Authority]): Authority = existing match {
    case Some(authority) => Authority(input.user, input.refRoles, authority.invalidated, authority.id)
    case None => Authority(input.user, input.refRoles)
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    /*case Update => PartialSecureBlock(authority.update)
    case GetAll => PartialSecureBlock(authority.getAll)
    case Get => PartialSecureBlock(authority.get)
    case _ => PartialSecureBlock(god)*/
    case _ => NonSecureBlock
  }

  override protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[Authority]): Try[Set[Authority]] = {
    import AuthorityController._
    import store.sparql.select
    import store.sparql.select._

    val lwm = LWMPrefix[repository.Rdf]
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

  override def allAtomic(securedContext: SecureContext = contextFrom(GetAll)): Action[AnyContent] = Action { request =>
    filtered(request)(Set.empty)
      .flatMap { set =>
        if (set.nonEmpty)
          retrieveLots[AuthorityAtom](set map Authority.generateUri)
        else
          retrieveAll[AuthorityAtom]
      }
      .map(set => chunk(set))
      .mapResult(enum => Ok.stream(enum).as(mimeType))
  }
}
