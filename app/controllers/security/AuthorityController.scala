package controllers.security

import java.util.UUID

import controllers.crud._
import models.security.Permissions._
import models.security._
import models.users.User
import models.{Course, UriGenerator}
import org.openrdf.model.Value
import org.w3.banana.RDFPrefix
import org.w3.banana.sesame.Sesame
import play.api.libs.json._
import play.api.mvc.{Action, AnyContent}
import services.{RoleService, SessionHandlingService}
import store.Prefixes.LWMPrefix
import store.bind.Descriptor.Descriptor
import store.sparql.Clause
import store.{Namespace, SesameRepository}
import utils.{LwmMimeType, Return}
import utils.Ops.MonadInstances.listM

import scala.collection.Map
import scala.util.{Failure, Success, Try}

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

  override protected def coAtomic(atom: AuthorityAtom): Authority = Authority(atom.user.id, atom.role.id, atom.course.map(_.id), atom.invalidated, atom.id)

  override protected def compareModel(input: AuthorityProtocol, output: Authority): Boolean = input.course == output.course && input.role == output.role

  override protected def fromInput(input: AuthorityProtocol, existing: Option[Authority]): Authority = existing match {
    case Some(authority) => Authority(input.user, input.role, input.course, authority.invalidated, authority.id)
    case None => Authority(input.user, input.role, input.course)
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Create => PartialSecureBlock(authority.create)
    case Delete => PartialSecureBlock(authority.delete)
    case GetAll => PartialSecureBlock(authority.getAll)
    case Get => PartialSecureBlock(authority.get)
    case _ => PartialSecureBlock(god)
  }

  override protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[Authority]): Try[Set[Authority]] = {
    import controllers.security.AuthorityController._
    import store.sparql.select
    import store.sparql.select._

    val lwm = LWMPrefix[repository.Rdf]
    val rdf = RDFPrefix[repository.Rdf]

    val initClause: Clause = **(v("auth"), p(rdf.`type`), s(lwm.Authority))

    queryString.foldLeft(Try(initClause)) {
      case (authorities, (`courseAttribute`, courses)) => authorities map {
        _ append **(v("auth"), p(lwm.course), s(Course.generateUri(UUID.fromString(courses.head))))
      }
      case (authorities, (`roleAttribute`, roles)) => authorities map {
        _ append **(v("auth"), p(lwm.role), s(Role.generateUri(UUID.fromString(roles.head))))
      }
      case (authorities, (`userAttribute`, users)) => authorities map {
        _ append **(v("auth"), p(lwm.privileged), s(User.generateUri(UUID.fromString(users.head))))
      }
      case _ => Failure(new Throwable("Unknown attribute"))
    } flatMap { clause =>
      val query = select distinct "auth" where clause

      repository.prepareQuery(query).
        select(_.get("auth")).
        transform(_.fold(List.empty[Value])(identity)).
        map(_.stringValue()).
        requestAll(repository.getMany[Authority]).
        run
    }
  }

  def delete(id: String) = contextFrom(Delete) action { request =>
    import models.security.Roles.{Employee, Student}

    val uri = asUri(namespace, request)

    optional(repository.get[AuthorityAtom](uri)).
      when(auth => !(auth.role.label == Employee || auth.role.label == Student), _ => invalidate[Authority](uri)) {
      PreconditionFailed(Json.obj(
        "status" -> "KO",
        "message" -> s"The user associated with $id have to remain with at least one basic role, namely $Student or $Employee"
      ))
    }.mapResult(_ => Ok(Json.obj("status" -> "OK")))
  }

  override def allAtomic(securedContext: SecureContext = contextFrom(GetAll)): Action[AnyContent] = securedContext action { request =>
    filter(request)(Set.empty)
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
