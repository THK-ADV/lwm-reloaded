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

  override implicit def rdfReads: FromPG[Sesame, Authority] = defaultBindings.AuthorityBinding.authorityBinder

  override implicit def classUrisFor: ClassUrisFor[Sesame, Authority] = defaultBindings.AuthorityBinding.classUri

  override implicit def uriGenerator: UriGenerator[Authority] = Authority

  override implicit def rdfWrites: ToPG[Sesame, Authority] = defaultBindings.AuthorityBinding.authorityBinder

  override implicit def reads: Reads[AuthorityProtocol] = Authority.reads

  override implicit def writes: Writes[Authority] = Authority.writes

  override protected def fromInput(input: AuthorityProtocol, existing: Option[Authority]): Authority = existing match {
    case Some(authority) => Authority(input.user, input.refRoles, authority.id)
    case None => Authority(input.user, input.refRoles, Authority.randomUUID)
  }

  override implicit val mimeType: LwmMimeType = LwmMimeType.authorityV1Json

  override protected def atomize(output: Authority): Try[Option[JsValue]] = {
    import defaultBindings.RefRoleBinding._
    import defaultBindings.UserBinding._
    import models.security.Authority._

    implicit val ns = repository.namespace
    for {
      maybeUser <- repository.get[User](User.generateUri(output.user))(userBinder)
      refroles <- repository.getMany[RefRole](output.refRoles map RefRole.generateUri)
      atomicRefRoles <- atomRefs(refroles)
    } yield maybeUser map { user =>
      Json.toJson(AuthorityAtom(user, atomicRefRoles, output.id))
    }
  }

  import defaultBindings.CourseBinding._
  import defaultBindings.EmployeeBinding._
  import defaultBindings.RoleBinding._

  def atomRefs(s: Set[RefRole]): Try[Set[RefRoleAtom]] = {
    import scalaz.syntax.applicative._

    s.foldLeft(Try(Set.empty[RefRoleAtom])) { (T, ref) =>
      for {
        maybeRole <- repository.get[Role](Role.generateUri(ref.role)(namespace))
        course <- Try(ref.course).flatPeek(course => repository.get[Course](Course.generateUri(course)(namespace)))
        lecturer <- Try(course).flatPeek(c => repository.get[Employee](User.generateUri(c.lecturer)(namespace)))
        courseAtom = (course |@| lecturer) ((c, e) => CourseAtom(c.label, c.description, c.abbreviation, e, c.semesterIndex, c.id))
        set <- T
      } yield maybeRole match {
        case Some(role) => set + RefRoleAtom(courseAtom, role, ref.id)
        case None => set
      }
    }
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Update => PartialSecureBlock(authority.update)
    case GetAll => PartialSecureBlock(authority.getAll)
    case Get => PartialSecureBlock(authority.get)
    case _ => PartialSecureBlock(god)
  }


  // GET /ts with optional queries and deserialisation
  override def allAtomic(securedContext: SecureContext = contextFrom(GetAll)): Action[AnyContent] = securedContext action { request =>
    val res = {
      if (request.queryString.nonEmpty)
        getWithFilter(request.queryString)(Set.empty) map (set => chunkAtoms(set))
      else {
        repository.get[Authority] map (set => chunkAtoms(set))
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
    val clause: Clause = ^(v("auth"), p(lwm.refroles), v("refs"))

    queryString.foldRight(Try(clause)) {
      case ((`courseAttribute`, set), t) => t map {
        _ append ^(v("refs"), p(lwm.course), s(Course.generateUri(UUID.fromString(set.head))))
      }
      case ((`roleAttribute`, set), t) => t map {
        _ append ^(v("refs"), p(lwm.role), s(Role.generateUri(UUID.fromString(set.head))))
      }
      case ((`userAttribute`, set), t) => t map {
        _ append ^(v("auth"), p(lwm.privileged), s(User.generateUri(UUID.fromString(set.head))))
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
