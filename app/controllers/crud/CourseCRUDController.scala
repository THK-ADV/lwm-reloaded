package controllers.crud

import java.util.UUID

import models.users.{Employee, User}
import models.{Course, CourseAtom, CourseProtocol, UriGenerator}
import org.w3.banana.RDFPrefix
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json._
import services.{RoleService, SessionHandlingService}
import store.Prefixes.LWMPrefix
import store.sparql.SelectClause
import store.{Namespace, SesameRepository}
import utils.LwmMimeType
import models.security.Permissions._
import models.security.{Authority, RefRole, Role}
import models.security.Roles._
import play.api.mvc.{Request, Result}
import store.bind.Descriptor.{CompositeClassUris, Descriptor}

import scala.collection.Map
import scala.util.{Failure, Success, Try}
import store.sparql.select
import store.sparql.select._

object CourseCRUDController {
  val lecturerAttribute = "lecturer"
}

class CourseCRUDController(val repository: SesameRepository, val sessionService: SessionHandlingService, val namespace: Namespace, val roleService: RoleService) extends AbstractCRUDController[CourseProtocol, Course, CourseAtom] {

  override implicit def descriptor: Descriptor[Sesame, Course] = defaultBindings.CourseDescriptor

  override implicit def descriptorAtom: Descriptor[Sesame, CourseAtom] = defaultBindings.CourseAtomDescriptor

  override implicit def uriGenerator: UriGenerator[Course] = Course

  override implicit def reads: Reads[CourseProtocol] = Course.reads

  override implicit def writes: Writes[Course] = Course.writes

  override implicit def writesAtom: Writes[CourseAtom] = Course.writesAtom

  override protected def fromInput(input: CourseProtocol, existing: Option[Course]): Course = existing match {
    case Some(course) => Course(input.label, input.description, input.abbreviation, input.lecturer, input.semesterIndex, course.id)
    case None => Course(input.label, input.description, input.abbreviation, input.lecturer, input.semesterIndex, Course.randomUUID)
  }

  override protected def coatomic(atom: CourseAtom): Course =
    Course(
      atom.label,
      atom.description,
      atom.abbreviation,
      atom.lecturer.id,
      atom.semesterIndex,
      atom.id)

  override val mimeType: LwmMimeType = LwmMimeType.courseV1Json

  override def getWithFilter(queryString: Map[String, Seq[String]])(courses: Set[Course]): Try[Set[Course]] = {
    import CourseCRUDController._

    queryString.foldRight(Try[Set[Course]](courses)) {
      case ((`lecturerAttribute`, v), t) => t flatMap (set => Try(UUID.fromString(v.head)).map(p => set.filter(_.lecturer == p)))
      case ((_, _), set) => Failure(new Throwable("Unknown attribute"))
    }
  }

  override protected def existsQuery(input: CourseProtocol): (SelectClause, Var) = {
    lazy val prefixes = LWMPrefix[repository.Rdf]
    lazy val rdf = RDFPrefix[repository.Rdf]

    (select("id") where {
      **(v("s"), p(rdf.`type`), s(prefixes.Course)).
        **(v("s"), p(prefixes.label), o(input.label)).
        **(v("s"), p(prefixes.lecturer), s(User.generateUri(input.lecturer)(namespace))).
        **(v("s"), p(prefixes.id), v("id"))
    }, v("id"))
  }

  def updateFrom(course: String) = restrictedContext(course)(Update) asyncContentTypedAction { request =>
    super.update(course, NonSecureBlock)(request)
  }

  def updateAtomicFrom(course: String) = restrictedContext(course)(Update) asyncContentTypedAction { request =>
    super.updateAtomic(course, NonSecureBlock)(request)
  }

  def createWithRoles(secureContext: SecureContext = contextFrom(Create)) = secureContext contentTypedAction { request =>
    validate(request)
      .flatMap(existence)
      .flatMap(withRoles)
      .mapResult(c => Created(Json.toJson(c)).as(mimeType))
  }

  def createAtomicWithRoles(secureContext: SecureContext = contextFrom(Create)) = secureContext contentTypedAction { request =>
    val uri = s"$namespace${request.uri}".replaceAll("/atomic", "")
    validate(request)
      .flatMap(existence)
      .flatMap(withRoles)
      .flatMap(a => retrieve[CourseAtom](uri + s"/${a.id}"))
      .mapResult(ca => Created(Json.toJson(ca)).as(mimeType))
  }

  private def withRoles(course: Course) = {
    import defaultBindings.{RefRoleDescriptor, AuthorityDescriptor}

    import utils.Ops.MonadInstances._
    import utils.Ops.TraverseInstances._
    import utils.Ops._
    import scalaz.syntax.applicative._

    /*
     * When you create a course, you also create the refroles associated with that course
     * and proceed to expand the lecturer's authority up to RightsManager and CourseManager
     *  => Create CourseManager, CourseEmployee, CourseAssistant for new Course
     *  => Update Lecturer Authority with RightsManager and CourseManager
     *
     *  TODO: There should be a better way of doing this bloody thing
     */
    (for {
      roles <- roleService.rolesByLabel(CourseManager, CourseEmployee, CourseAssistant) if roles.nonEmpty
      rvRefRole <- roleService.refRoleByLabel(RightsManager)
      lecturerAuth <- roleService.authorityFor(course.lecturer.toString) if lecturerAuth.isDefined
      refRoles = roles.map(role => RefRole(Some(course.id), role.id))
      mvRefRole = roles.find(_.label == CourseManager).flatMap(r => refRoles.find(_.role == r.id))
      authority = (lecturerAuth |@| rvRefRole |@| mvRefRole) ((authority, rv, mv) => Authority(authority.user, authority.refRoles + mv.id + rv.id, authority.id))
      _ <- authority.map(repository.update(_)(AuthorityDescriptor, Authority)).sequenceM
      _ <- repository.add[Course](course)
      _ <- repository.addMany[RefRole](refRoles)
    } yield course) match {
      case Success(a) => Continue(a)
      case Failure(e) =>
        Stop(
          InternalServerError(Json.obj(
            "status" -> "KO",
            "errors" -> e.getMessage
          )))
    }
  }

  override protected def compareModel(input: CourseProtocol, output: Course): Boolean = {
    input.description == output.description && input.abbreviation == output.abbreviation && input.semesterIndex == output.semesterIndex
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(course.get)
    case GetAll => PartialSecureBlock(course.getAll)
    case _ => PartialSecureBlock(prime)
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case Update => SecureBlock(restrictionId, course.update)
    case _ => PartialSecureBlock(god)
  }
}
