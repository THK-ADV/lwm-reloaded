package controllers.crud

import java.util.UUID

import models.users.User
import models.{Course, CourseAtom, CourseProtocol, UriGenerator}
import org.w3.banana.RDFPrefix
import org.w3.banana.sesame.Sesame
import play.api.libs.json._
import services.{RoleService, SessionHandlingService}
import store.Prefixes.LWMPrefix
import store.sparql.SelectClause
import store.{Namespace, SesameRepository}
import utils.{Continue, LwmMimeType, Return}
import models.security.Permissions._
import models.security.{Authority, RefRole}
import models.security.Roles._
import store.bind.Descriptor.Descriptor
import scala.collection.Map
import scala.util.{Failure, Success, Try}
import store.sparql.select
import store.sparql.select._

object CourseCRUDController {
  val lecturerAttribute = "lecturer"
}

class CourseCRUDController(val repository: SesameRepository, val sessionService: SessionHandlingService, implicit val namespace: Namespace, val roleService: RoleService) extends AbstractCRUDController[CourseProtocol, Course, CourseAtom] {

  override val mimeType: LwmMimeType = LwmMimeType.courseV1Json

  override implicit val descriptor: Descriptor[Sesame, Course] = defaultBindings.CourseDescriptor

  override implicit val descriptorAtom: Descriptor[Sesame, CourseAtom] = defaultBindings.CourseAtomDescriptor

  override implicit val reads: Reads[CourseProtocol] = Course.reads

  override implicit val writes: Writes[Course] = Course.writes

  override implicit val writesAtom: Writes[CourseAtom] = Course.writesAtom

  override implicit val uriGenerator: UriGenerator[Course] = Course

  override protected def coatomic(atom: CourseAtom): Course =
    Course(
      atom.label,
      atom.description,
      atom.abbreviation,
      atom.lecturer.id,
      atom.semesterIndex,
      atom.invalidated,
      atom.id)

  override protected def compareModel(input: CourseProtocol, output: Course): Boolean = {
    input.description == output.description && input.abbreviation == output.abbreviation && input.semesterIndex == output.semesterIndex
  }

  override protected def fromInput(input: CourseProtocol, existing: Option[Course]): Course = existing match {
    case Some(course) => Course(input.label, input.description, input.abbreviation, input.lecturer, input.semesterIndex, None, course.id)
    case None => Course(input.label, input.description, input.abbreviation, input.lecturer, input.semesterIndex)
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

  override def getWithFilter(queryString: Map[String, Seq[String]])(courses: Set[Course]): Try[Set[Course]] = {
    import CourseCRUDController._

    queryString.foldRight(Try[Set[Course]](courses)) {
      case ((`lecturerAttribute`, v), t) => t flatMap (set => Try(UUID.fromString(v.head)).map(p => set.filter(_.lecturer == p)))
      case ((_, _), set) => Failure(new Throwable("Unknown attribute"))
    }
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
      .mapResult(course => Created(Json.toJson(course)).as(mimeType))
  }

  def createAtomicWithRoles(secureContext: SecureContext = contextFrom(Create)) = secureContext contentTypedAction { request =>
    validate(request)
      .flatMap(existence)
      .flatMap(withRoles)
      .flatMap(course => retrieve[CourseAtom](Course.generateUri(course)))
      .mapResult(courseAtom => Created(Json.toJson(courseAtom)).as(mimeType))
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
      rvRefRole <- roleService.refRolesByLabel(RightsManager) map (_.headOption)
      lecturerAuth <- roleService.authorityFor(course.lecturer.toString) if lecturerAuth.isDefined
      refRoles = roles.map(role => RefRole(Some(course.id), role.id))
      mvRefRole = roles.find(_.label == CourseManager).flatMap(r => refRoles.find(_.role == r.id))
      authority = (lecturerAuth |@| rvRefRole |@| mvRefRole) ((authority, rv, mv) => Authority(authority.user, authority.refRoles + mv.id + rv.id, authority.invalidated, authority.id))
      _ <- authority.map(repository.update(_)(AuthorityDescriptor, Authority)).sequenceM
      _ <- repository.add[Course](course)
      _ <- repository.addMany[RefRole](refRoles)
    } yield course) match {
      case Success(a) => Continue(a)
      case Failure(e) =>
        println(e)
        Return(
          InternalServerError(Json.obj(
            "status" -> "KO",
            "errors" -> e.getMessage
          )))
    }
  }
}
