package controllers.crud

import java.util.UUID

import models.{Degree, DegreeProtocol, UriGenerator}
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.Result
import services.RoleService
import store.{Namespace, SesameRepository}
import utils.LwmMimeType

import scala.collection.Map

class DegreeCRUDController(val repository: SesameRepository, val namespace: Namespace, val roleService: RoleService) extends AbstractCRUDController[DegreeProtocol, Degree]{
  override implicit def reads: Reads[DegreeProtocol] = Degree.reads

  override implicit def writes: Writes[Degree] = Degree.writes

  override implicit def rdfReads: FromPG[Sesame, Degree] = defaultBindings.DegreeBinding.degreeBinder

  override implicit def uriGenerator: UriGenerator[Degree] = Degree

  override implicit def rdfWrites: ToPG[Sesame, Degree] = defaultBindings.DegreeBinding.degreeBinder

  override implicit def classUrisFor: ClassUrisFor[Sesame, Degree] = defaultBindings.DegreeBinding.classUri

  override protected def fromInput(input: DegreeProtocol, id: Option[UUID]): Degree = id match {
    case Some(uuid) => Degree(input.label, input.abbreviation, uuid)
    case None => Degree(input.label, input.abbreviation, Degree.randomUUID)
  }

  override def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[Degree]): Result = ???

  override val mimeType: LwmMimeType = LwmMimeType.degreeV1Json

  override protected def duplicate(input: DegreeProtocol, output: Degree): Boolean = {
    input.label == output.label && input.abbreviation == output.abbreviation
  }
}