package controllers.crud

import models.{Course, UriGenerator}
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.Result
import store.{Namespace, SesameRepository}

import scala.collection.Map

class CourseCRUDController(val repository: SesameRepository, val namespace: Namespace) extends AbstractCRUDController[Course] {
  override implicit def rdfWrites: ToPG[Sesame, Course] = defaultBindings.CourseBinding.courseBinder

  override implicit def rdfReads: FromPG[Sesame, Course] = defaultBindings.CourseBinding.courseBinder

  override implicit def classUrisFor: ClassUrisFor[Sesame, Course] = defaultBindings.CourseBinding.classUri

  override implicit def uriGenerator: UriGenerator[Course] = Course

  override implicit def reads: Reads[Course] = Course.reads

  override implicit def writes: Writes[Course] = Course.writes

  override def getWithFilter(queryString: Map[String, Seq[String]]): Result = ???
}
