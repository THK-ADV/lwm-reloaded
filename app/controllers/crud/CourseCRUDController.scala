package controllers.crud

import java.util.UUID

import models.{Course, CourseProtocol, UriGenerator}
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.Result
import services.RoleService
import store.{Namespace, SesameRepository}
import utils.LwmMimeType

import scala.collection.Map

class CourseCRUDController(val repository: SesameRepository, val namespace: Namespace, val roleService: RoleService) extends AbstractCRUDController[CourseProtocol, Course] {
  override implicit def rdfWrites: ToPG[Sesame, Course] = defaultBindings.CourseBinding.courseBinder

  override implicit def rdfReads: FromPG[Sesame, Course] = defaultBindings.CourseBinding.courseBinder

  override implicit def classUrisFor: ClassUrisFor[Sesame, Course] = defaultBindings.CourseBinding.classUri

  override implicit def uriGenerator: UriGenerator[Course] = Course

  override implicit def reads: Reads[CourseProtocol] = Course.reads

  override implicit def writes: Writes[Course] = Course.writes

  override protected def fromInput(input: CourseProtocol, id: Option[UUID]): Course = id match {
    case Some(uuid) => Course(input.label, input.lecturer, uuid)
    case None => Course(input.label, input.lecturer, Course.randomUUID)
  }

  override def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[Course]): Result = ???

  override val mimeType: LwmMimeType = LwmMimeType.courseV1Json
}
