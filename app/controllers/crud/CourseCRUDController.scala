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
import models.security.Authority
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

  override protected def coAtomic(atom: CourseAtom): Course = Course(
    atom.label,
    atom.description,
    atom.abbreviation,
    atom.lecturer.id,
    atom.semesterIndex,
    atom.invalidated,
    atom.id
  )


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
    lazy val lwm = LWMPrefix[repository.Rdf]
    lazy val rdf = RDFPrefix[repository.Rdf]

    (select("id") where {
      **(v("s"), p(rdf.`type`), s(lwm.Course)).
        **(v("s"), p(lwm.label), o(input.label)).
        **(v("s"), p(lwm.lecturer), s(User.generateUri(input.lecturer)(namespace))).
        **(v("s"), p(lwm.id), v("id"))
    }, v("id"))
  }

  override def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[Course]): Try[Set[Course]] = {
    import controllers.crud.CourseCRUDController._

    queryString.foldLeft(Try(all)) {
      case (courses, (`lecturerAttribute`, lecturers)) => courses flatMap (set => Try(UUID.fromString(lecturers.head)).map(p => set.filter(_.lecturer == p)))
      case (_, (_, _)) => Failure(new Throwable("Unknown attribute"))
    }
  }

  def updateFrom(course: String) = restrictedContext(course)(Update) asyncContentTypedAction { implicit request =>
    update(course, NonSecureBlock)(rebase(course))
  }

  def updateAtomicFrom(course: String) = restrictedContext(course)(Update) asyncContentTypedAction { implicit request =>
    updateAtomic(course, NonSecureBlock)(rebase(course))
  }

  def createWithRoles = contextFrom(Create) contentTypedAction { request =>
    validate(request)
      .flatMap(existence)
      .flatMap(withRoles)
      .mapResult{ course => println(course); Created(Json.toJson(course)).as(mimeType) }
  }

  def createAtomicWithRoles = contextFrom(Create) contentTypedAction { request =>
    validate(request)
      .flatMap(existence)
      .flatMap(withRoles)
      .flatMap(course => retrieve[CourseAtom](Course.generateUri(course)))
      .mapResult(courseAtom => Created(Json.toJson(courseAtom)).as(mimeType))
  }

  /**
    * When you create a course, you also expand the lecturer's authority up to RightsManager and CourseManager
    *
    * @param course which is associated
    * @return
    */
  private def withRoles(course: Course) = {
    import defaultBindings.AuthorityDescriptor

    (for {
      roles <- roleService.rolesByLabel(CourseManager, RightsManager) if roles.nonEmpty
      authorities = roles.map { role =>
        val optCourse = if (role.label == CourseManager) Some(course.id) else None
        Authority(course.lecturer, role.id, optCourse)
      }
      _ <- repository.addMany[Authority](authorities)
      _ <- repository.add[Course](course)
    } yield course) match {
      case Success(c) => Continue(c)
      case Failure(e) =>
        Return(
          InternalServerError(Json.obj(
            "status" -> "KO",
            "errors" -> e.getMessage
          )))
    }
  }
}
