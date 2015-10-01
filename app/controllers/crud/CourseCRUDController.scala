package controllers.crud

import java.util.UUID

import models.{CourseProtocol, Course, UriGenerator}
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Json, Reads, Writes}
import play.api.mvc.Result
import store.{Namespace, SesameRepository}
import utils.LWMMimeType

import scala.collection.Map
import scala.util.{Failure, Success, Try}

object CourseCRUDController {
  val lecturerAttribute = "lecturer"
}

class CourseCRUDController(val repository: SesameRepository, val namespace: Namespace) extends AbstractCRUDController[CourseProtocol, Course] {
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

  override val mimeType: LWMMimeType = LWMMimeType.courseV1Json

  override def getWithFilter(queryString: Map[String, Seq[String]])(courses: Set[Course]): Result = {
    val attribute = queryString.get(CourseCRUDController.lecturerAttribute)

    attribute match {
      case Some(queryParameter) =>
        val uuid = Try(UUID.fromString(queryParameter.head))

        uuid match {
          case Success(s) =>
            val byLecturer = courses.filter(c => c.lecturer == s)

            if (byLecturer.isEmpty) {
              NotFound(Json.obj(
                "status" -> "KO",
                "message" -> "No such element..."
              ))
            } else {
              Ok(Json.toJson(byLecturer)).as(mimeType)
            }

          case Failure(e) =>
            BadRequest(Json.obj(
              "status" -> "KO",
              "errors" -> e.getMessage
            ))
        }

      case None =>
        ServiceUnavailable(Json.obj(
          "status" -> "KO",
          "message" -> "query attribute not found"
        ))
    }
  }
}
