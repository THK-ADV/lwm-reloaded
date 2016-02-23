package controllers.crud

import java.util.UUID

import models.users.{User, Employee}
import models.{Course, CourseProtocol, UriGenerator}
import org.openrdf.model.Value
import org.w3.banana.RDFPrefix
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Json, Reads, Writes}
import play.api.mvc.Result
import services.RoleService
import store.Prefixes.LWMPrefix
import store.sparql.SelectClause
import store.{Namespace, SesameRepository}
import utils.LwmMimeType

import scala.collection.Map
import scala.util.{Failure, Success, Try}
import store.sparql.select
import store.sparql.select._
object CourseCRUDController {
  val lecturerAttribute = "lecturer"
}

class CourseCRUDController(val repository: SesameRepository, val namespace: Namespace, val roleService: RoleService) extends AbstractCRUDController[CourseProtocol, Course] {
  override implicit def rdfWrites: ToPG[Sesame, Course] = defaultBindings.CourseBinding.courseBinder

  override implicit def rdfReads: FromPG[Sesame, Course] = defaultBindings.CourseBinding.courseBinder

  override implicit def classUrisFor: ClassUrisFor[Sesame, Course] = defaultBindings.CourseBinding.classUri

  override implicit def uriGenerator: UriGenerator[Course] = Course

  override implicit def reads: Reads[CourseProtocol] = Course.reads

  override implicit def writes: Writes[Course] = Course.writes

  override protected def fromInput(input: CourseProtocol, id: Option[UUID]): Course = id match {
    case Some(uuid) => Course(input.label, input.description, input.abbreviation, input.lecturer, input.semesterIndex, uuid)
    case None => Course(input.label, input.description, input.abbreviation, input.lecturer, input.semesterIndex, Course.randomUUID)
  }

  override val mimeType: LwmMimeType = LwmMimeType.courseV1Json

  override def getWithFilter(queryString: Map[String, Seq[String]])(courses: Set[Course]): Result = {
    import CourseCRUDController._

    val filtered = queryString.foldRight(Try[Set[Course]](courses)) {
      case ((`lecturerAttribute`, v), t) => t flatMap (set => Try(UUID.fromString(v.head)).map(p => set.filter(_.lecturer == p)))
      case ((_, _), set) => Failure(new Throwable("Unknown attribute"))
    }

    filtered match {
      case Success(s) =>
        if (s.isEmpty)
          NotFound(Json.obj(
            "status" -> "KO",
            "message" -> "No such element..."
          ))
        else
          Ok(Json.toJson(s)).as(mimeType)

      case Failure(e) => BadRequest(Json.obj(
        "status" -> "KO",
        "message" -> e.getMessage
      ))
    }
  }

  override protected def existsQuery(input: CourseProtocol): (SelectClause, Var) = {
    lazy val prefixes = LWMPrefix[repository.Rdf]
    lazy val rdf = RDFPrefix[repository.Rdf]

    (select ("id") where {
      ^(v("s"), p(rdf.`type`), s(prefixes.Course)) .
        ^(v("s"), p(prefixes.label), o(input.label)) .
        ^(v("s"), p(prefixes.description), o(input.description)) .
        ^(v("s"), p(prefixes.id), v("id"))
    }, v("id"))
  }

  override protected def compareModel(input: CourseProtocol, output: Course): Boolean = {
    input.label == output.label && input.description == output.description
  }
}
