package controllers.crud

import java.util.UUID

import models.{CourseProtocol, Course, UriGenerator}
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.Result
import store.{Namespace, SesameRepository}
import utils.LWMMimeType

import scala.collection.Map

class CourseCRUDController(val repository: SesameRepository, val namespace: Namespace) extends AbstractCRUDController[CourseProtocol, Course] {
  override implicit def rdfWrites: ToPG[Sesame, Course] = defaultBindings.CourseBinding.courseBinder

  override implicit def rdfReads: FromPG[Sesame, Course] = defaultBindings.CourseBinding.courseBinder

  override implicit def classUrisFor: ClassUrisFor[Sesame, Course] = defaultBindings.CourseBinding.classUri

  override implicit def uriGenerator: UriGenerator[Course] = Course

  override implicit def reads: Reads[CourseProtocol] = Course.reads

  override implicit def writes: Writes[Course] = Course.writes

  override def getWithFilter(queryString: Map[String, Seq[String]]): Result = ???

  override protected def fromInput(input: CourseProtocol, id: Option[UUID]): Course = id match {
    case Some(uuid) => Course(input.label, input.lecturer, uuid)
    case None => Course(input.label, input.lecturer, Course.randomUUID)
  }

  override def mimeType: LWMMimeType = LWMMimeType.courseV1Json
}
