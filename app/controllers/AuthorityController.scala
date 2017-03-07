package controllers

import java.util.UUID

import models.Permissions.{authority, god}
import models._
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
import utils.LwmMimeType
import utils.Ops.MonadInstances.listM
import controllers.AuthorityController._
import scala.collection.Map
import scala.util.{Failure, Try}

object AuthorityController {
  val userAttribute = "user"
  val courseAttribute = "course"
  val roleAttribute = "role"
}

class AuthorityController(val repository: SesameRepository, val sessionService: SessionHandlingService, implicit val namespace: Namespace, val roleService: RoleService) extends AbstractCRUDController[SesameAuthorityProtocol, SesameAuthority, SesameAuthorityAtom] {

  override implicit val mimeType: LwmMimeType = LwmMimeType.authorityV1Json

  override implicit val descriptor: Descriptor[Sesame, SesameAuthority] = defaultBindings.AuthorityDescriptor

  override implicit val descriptorAtom: Descriptor[Sesame, SesameAuthorityAtom] = defaultBindings.AuthorityAtomDescriptor

  override implicit val reads: Reads[SesameAuthorityProtocol] = SesameAuthority.reads

  override implicit val writes: Writes[SesameAuthority] = SesameAuthority.writes

  override implicit val writesAtom: Writes[SesameAuthorityAtom] = SesameAuthority.writesAtom

  override implicit val uriGenerator: UriGenerator[SesameAuthority] = SesameAuthority

  override protected def coAtomic(atom: SesameAuthorityAtom): SesameAuthority = SesameAuthority(atom.user.id, atom.role.id, atom.course.map(_.id), atom.invalidated, atom.id)

  override protected def compareModel(input: SesameAuthorityProtocol, output: SesameAuthority): Boolean = input.course == output.course && input.role == output.role

  override protected def fromInput(input: SesameAuthorityProtocol, existing: Option[SesameAuthority]): SesameAuthority = existing match {
    case Some(authority) => SesameAuthority(input.user, input.role, input.course, authority.invalidated, authority.id)
    case None => SesameAuthority(input.user, input.role, input.course)
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Create => PartialSecureBlock(authority.create)
    case Delete => PartialSecureBlock(authority.delete)
    case GetAll => PartialSecureBlock(authority.getAll)
    case Get => PartialSecureBlock(authority.get)
    case _ => PartialSecureBlock(god)
  }

  override protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[SesameAuthority]): Try[Set[SesameAuthority]] = {
    import store.sparql.select
    import store.sparql.select._

    val lwm = LWMPrefix[repository.Rdf]
    val rdf = RDFPrefix[repository.Rdf]

    val initClause: Clause = **(v("auth"), p(rdf.`type`), s(lwm.Authority))

    queryString.foldLeft(Try(initClause)) {
      case (authorities, (`courseAttribute`, courses)) => authorities map {
        _ append **(v("auth"), p(lwm.course), s(SesameCourse.generateUri(UUID.fromString(courses.head))))
      }
      case (authorities, (`roleAttribute`, roles)) => authorities map {
        _ append **(v("auth"), p(lwm.role), s(SesameRole.generateUri(UUID.fromString(roles.head))))
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
        requestAll(repository.getMany[SesameAuthority]).
        run
    }
  }

  def delete(id: String) = contextFrom(Delete) action { request =>
    import models.Roles.{EmployeeLabel, StudentLabel}

    val uri = asUri(namespace, request)

    optional(repository.get[SesameAuthorityAtom](uri)).
      when(auth => !(auth.role.label == EmployeeLabel || auth.role.label == StudentLabel), _ => invalidate[SesameAuthority](uri)) {
      PreconditionFailed(Json.obj(
        "status" -> "KO",
        "message" -> s"The user associated with $id have to remain with at least one basic role, namely $StudentLabel or $EmployeeLabel"
      ))
    }.mapResult(_ => Ok(Json.obj("status" -> "OK")))
  }

  override def allAtomic(securedContext: SecureContext = contextFrom(GetAll)): Action[AnyContent] = securedContext action { request =>
    filter(request)(Set.empty)
      .flatMap { set =>
        if (set.nonEmpty)
          retrieveLots[SesameAuthorityAtom](set map SesameAuthority.generateUri)
        else
          retrieveAll[SesameAuthorityAtom]
      }
      .map(set => chunk(set))
      .mapResult(enum => Ok.stream(enum).as(mimeType))
  }
}
