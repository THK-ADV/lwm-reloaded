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
import play.api.mvc.Result

import scala.collection.Map
import scala.util.{Failure, Success, Try}
import store.sparql.select
import store.sparql.select._

object CourseCRUDController {
  val lecturerAttribute = "lecturer"
}

class CourseCRUDController(val repository: SesameRepository, val sessionService: SessionHandlingService, val namespace: Namespace, val roleService: RoleService) extends AbstractCRUDController[CourseProtocol, Course] {
  override implicit def rdfWrites: ToPG[Sesame, Course] = defaultBindings.CourseBinding.courseBinder

  override implicit def rdfReads: FromPG[Sesame, Course] = defaultBindings.CourseBinding.courseBinder

  override implicit def classUrisFor: ClassUrisFor[Sesame, Course] = defaultBindings.CourseBinding.classUri

  override implicit def uriGenerator: UriGenerator[Course] = Course

  override implicit def reads: Reads[CourseProtocol] = Course.reads

  override implicit def writes: Writes[Course] = Course.writes

  override protected def fromInput(input: CourseProtocol, existing: Option[Course]): Course = existing match {
    case Some(course) => Course(input.label, input.description, input.abbreviation, input.lecturer, input.semesterIndex, course.id)
    case None => Course(input.label, input.description, input.abbreviation, input.lecturer, input.semesterIndex, Course.randomUUID)
  }

  override protected def atomize(output: Course): Try[Option[JsValue]] = {
    import utils.Ops._
    import utils.Ops.MonadInstances.{tryM, optM}
    import defaultBindings.EmployeeBinding.employeeBinder
    import Course.atomicWrites

    repository.get[Employee](User.generateUri(output.lecturer)(namespace)).peek { employee =>
      val atom = CourseAtom(output.label, output.description, output.abbreviation, employee, output.semesterIndex, output.id)
      Json.toJson(atom)
    }
  }

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

    (select ("id") where {
      **(v("s"), p(rdf.`type`), s(prefixes.Course)) .
        **(v("s"), p(prefixes.label), o(input.label)) .
        **(v("s"), p(prefixes.lecturer), s(User.generateUri(input.lecturer)(namespace))) .
        **(v("s"), p(prefixes.id), v("id"))
    }, v("id"))
  }

  def updateFrom(course: String) = restrictedContext(course)(Update) asyncContentTypedAction { request =>
    super.update(course, NonSecureBlock)(request)
  }

  def updateAtomicFrom(course: String) = restrictedContext(course)(Update) asyncContentTypedAction { request =>
    super.updateAtomic(course, NonSecureBlock)(request)
  }

  def createWithRoles(secureContext: SecureContext = contextFrom(Create)) = withRoles(secureContext) { course =>
    Success(Created(Json.toJson(course)).as(mimeType))
  }

  def createAtomicWithRoles(secureContext: SecureContext = contextFrom(Create)) = withRoles(secureContext) { course =>
    atomize(course) map {
      case Some(json) =>
        Created(json).as(mimeType)
      case None =>
        NotFound(Json.obj(
          "status" -> "KO",
          "message" -> "No such element..."
        ))
    }
  }

  private def withRoles(secureContext: SecureContext)(f: Course => Try[Result]) = secureContext contentTypedAction { request =>
    request.body.validate[CourseProtocol].fold(
      errors => {
        BadRequest(Json.obj(
          "status" -> "KO",
          "errors" -> JsError.toJson(errors)
        ))
      },
      success => existenceOf(success) { model =>
        import defaultBindings.CourseBinding._
        import defaultBindings.RoleBinding
        import defaultBindings.RefRoleBinding._
        import defaultBindings.AuthorityBinding._
        import store.sparql.select
        import store.sparql.select._

        lazy val lwm = LWMPrefix[repository.Rdf]
        lazy val rdf = RDFPrefix[repository.Rdf]

        val query = select ("s") where {
            **(v("s"), p(rdf.`type`), s(lwm.RefRole)) .
            **(v("s"), p(lwm.role), v("role")) .
            **(v("role"), p(lwm.label), o(RightsManager))
        }

        import utils.Ops.MonadInstances._
        import utils.Ops.NaturalTrasformations._
        import utils.Ops.TraverseInstances._
        import utils.Ops._
        import scalaz.syntax.applicative._

        val maybeRv = repository.prepareQuery(query).
          select(_.get("s")).
          changeTo(_.headOption).
          request[Option, RefRole](value => repository.get[RefRole](value.stringValue())).
          run

        for {
          allRoles <- repository.get[Role](RoleBinding.roleBinder, RoleBinding.classUri) if allRoles.nonEmpty
          rvRefRole <- maybeRv
          lecturerAuth <- roleService.authorityFor(model.lecturer.toString) if lecturerAuth.isDefined
          properRoles = allRoles.filter(role => (role.label == CourseManager) || (role.label == CourseEmployee) || (role.label == CourseAssistant))
          refRoles = properRoles.map(role => RefRole(Some(model.id), role.id))
          mvRefRole = properRoles.find(_.label == CourseManager).flatMap(r => refRoles.find(_.role == r.id))
          authority = (lecturerAuth |@| rvRefRole |@| mvRefRole)((authority, rv, mv) => Authority(authority.user, authority.refRoles + mv.id + rv.id, authority.id))
          _ <- authority.map(repository.update(_)(authorityBinder, Authority)).sequenceM
          _ <- repository.add[Course](model)
          _ <- repository.addMany[RefRole](refRoles)
          result <- f(model)
        } yield result
      }
    )
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
