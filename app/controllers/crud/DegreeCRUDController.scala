package controllers.crud

import java.util.UUID

import models.{DegreeProtocol, Degree, UriGenerator}
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.Result
import store.{Namespace, SesameRepository}
import utils.LWMMimeType

import scala.collection.Map

class DegreeCRUDController(val repository: SesameRepository, val namespace: Namespace) extends AbstractCRUDController[DegreeProtocol, Degree]{
  override implicit def reads: Reads[DegreeProtocol] = Degree.reads

  override implicit def writes: Writes[Degree] = Degree.writes

  override implicit def rdfReads: FromPG[Sesame, Degree] = defaultBindings.DegreeBinding.degreeBinder

  override implicit def uriGenerator: UriGenerator[Degree] = Degree

  override implicit def rdfWrites: ToPG[Sesame, Degree] = defaultBindings.DegreeBinding.degreeBinder

  override implicit def classUrisFor: ClassUrisFor[Sesame, Degree] = defaultBindings.DegreeBinding.classUri

  override def getWithFilter(queryString: Map[String, Seq[String]]): Result = ???

  override protected def fromInput(input: DegreeProtocol, id: Option[UUID]): Degree = ???

  override def mimeType: LWMMimeType = LWMMimeType.degreeV1Json
}