package controllers.crud

import java.util.UUID

import models.{Course, CourseProtocol, UriGenerator}
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Json, Reads, Writes}
import play.api.mvc.Result
import services.RoleService
import store.{Namespace, SesameRepository}
import utils.LwmMimeType

import scala.collection.Map
import scala.util.{Failure, Success, Try}

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
    case Some(uuid) => Course(input.label, input.abbreviation, input.lecturer, uuid)
    case None => Course(input.label, input.abbreviation, input.lecturer, Course.randomUUID)
  }

  override def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[Course]): Result = ???

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
}
