package controllers

import java.util.UUID

import models.{SesameAuthority, _}
import org.w3.banana.RDFPrefix
import org.w3.banana.sesame.Sesame
import play.api.libs.json._
import services.{RoleService, RoleServiceLike, SessionHandlingService}
import store.Prefixes.LWMPrefix
import store.bind.Descriptor.Descriptor
import store.sparql.select._
import store.sparql.{Clause, select}
import store.{Namespace, SesameRepository}
import utils.{Continue, LwmMimeType, Return}
import controllers.CourseCRUDController.lecturerAttribute

import scala.collection.Map
import scala.util.{Failure, Success, Try}
import models.Permissions.{course, god, prime}
import models.Roles.CourseManagerLabel

object CourseCRUDController {
  val lecturerAttribute = "lecturer"
}

class CourseCRUDController(val repository: SesameRepository, val sessionService: SessionHandlingService, implicit val namespace: Namespace, val roleService: RoleServiceLike) extends AbstractCRUDController[SesameCourseProtocol, SesameCourse, SesameCourseAtom] {

  override val mimeType: LwmMimeType = LwmMimeType.courseV1Json

  override implicit val descriptor: Descriptor[Sesame, SesameCourse] = defaultBindings.CourseDescriptor

  override implicit val descriptorAtom: Descriptor[Sesame, SesameCourseAtom] = defaultBindings.CourseAtomDescriptor

  override implicit val reads: Reads[SesameCourseProtocol] = SesameCourse.reads

  override implicit val writes: Writes[SesameCourse] = SesameCourse.writes

  override implicit val writesAtom: Writes[SesameCourseAtom] = SesameCourse.writesAtom

  override implicit val uriGenerator: UriGenerator[SesameCourse] = SesameCourse

  override protected def coAtomic(atom: SesameCourseAtom): SesameCourse = SesameCourse(
    atom.label,
    atom.description,
    atom.abbreviation,
    atom.lecturer.id,
    atom.semesterIndex,
    atom.invalidated,
    atom.id
  )


  override protected def compareModel(input: SesameCourseProtocol, output: SesameCourse): Boolean = {
    input.description == output.description && input.abbreviation == output.abbreviation && input.semesterIndex == output.semesterIndex
  }

  override protected def fromInput(input: SesameCourseProtocol, existing: Option[SesameCourse]): SesameCourse = existing match {
    case Some(course) => SesameCourse(input.label, input.description, input.abbreviation, input.lecturer, input.semesterIndex, None, course.id)
    case None => SesameCourse(input.label, input.description, input.abbreviation, input.lecturer, input.semesterIndex)
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

  override protected def existsQuery(input: SesameCourseProtocol): Clause = {
    lazy val lwm = LWMPrefix[repository.Rdf]
    lazy val rdf = RDFPrefix[repository.Rdf]

    select("s") where {
      **(v("s"), p(rdf.`type`), s(lwm.Course)).
        **(v("s"), p(lwm.label), o(input.label)).
        **(v("s"), p(lwm.lecturer), s(User.generateUri(input.lecturer)(namespace)))
    }
  }

  override def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[SesameCourse]): Try[Set[SesameCourse]] = {

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
    validateInput(request)
      .flatMap(existence)
      .flatMap(withRoles)
      .mapResult(course => Created(Json.toJson(course)).as(mimeType))
  }

  def createAtomicWithRoles = contextFrom(Create) contentTypedAction { request =>
    validateInput(request)
      .flatMap(existence)
      .flatMap(withRoles)
      .flatMap(course => retrieve[SesameCourseAtom](SesameCourse.generateUri(course)))
      .mapResult(courseAtom => Created(Json.toJson(courseAtom)).as(mimeType))
  }

  /**
    * When you create a course, you also expand the lecturer's authority up to RightsManager and CourseManager
    *
    * @param course which is associated
    * @return
    */
  private def withRoles(course: SesameCourse) = {
    import defaultBindings.AuthorityDescriptor

    (for {
      roles <- roleService.rolesForCourse(course.lecturer) if roles.nonEmpty
      authorities = roles.map { role =>
        val optCourse = if (role.label == CourseManagerLabel) Some(course.id) else None
        SesameAuthority(course.lecturer, role.id, optCourse)
      }
      _ <- repository.addMany[SesameAuthority](authorities)
      _ <- repository.add[SesameCourse](course)
    } yield course) match {
      case Success(c) => Continue(c)
      case Failure(e) => Return(
        InternalServerError(Json.obj(
          "status" -> "KO",
          "errors" -> e.getMessage
        )))
    }
  }
}
