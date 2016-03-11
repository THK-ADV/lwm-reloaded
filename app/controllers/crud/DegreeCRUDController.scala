package controllers.crud

import java.util.UUID

import models.security.Permissions._
import models.{Degree, DegreeProtocol, UriGenerator}
import org.w3.banana.RDFPrefix
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Json, JsValue, Reads, Writes}
import services.RoleService
import store.Prefixes.LWMPrefix
import store.sparql.{select, Clause}
import store.{Namespace, SesameRepository}
import utils.LwmMimeType

import scala.collection.Map
import scala.util.{Success, Try}

class DegreeCRUDController(val repository: SesameRepository, val namespace: Namespace, val roleService: RoleService) extends AbstractCRUDController[DegreeProtocol, Degree]{
  override implicit def reads: Reads[DegreeProtocol] = Degree.reads

  override implicit def writes: Writes[Degree] = Degree.writes

  override implicit def rdfReads: FromPG[Sesame, Degree] = defaultBindings.DegreeBinding.degreeBinder

  override implicit def uriGenerator: UriGenerator[Degree] = Degree

  override implicit def rdfWrites: ToPG[Sesame, Degree] = defaultBindings.DegreeBinding.degreeBinder

  override implicit def classUrisFor: ClassUrisFor[Sesame, Degree] = defaultBindings.DegreeBinding.classUri

  override protected def fromInput(input: DegreeProtocol, existing: Option[Degree]): Degree = existing match {
    case Some(degree) => Degree(input.label, input.abbreviation, degree.id)
    case None => Degree(input.label, input.abbreviation, Degree.randomUUID)
  }

  override val mimeType: LwmMimeType = LwmMimeType.degreeV1Json

  override protected def existsQuery(input: DegreeProtocol): (Clause, select.Var) = {
    lazy val prefixes = LWMPrefix[repository.Rdf]
    lazy val rdf = RDFPrefix[repository.Rdf]
    import store.sparql.select
    import store.sparql.select._

    (select ("id") where {
      ^(v("s"), p(rdf.`type`), s(prefixes.Degree)) .
        ^(v("s"), p(prefixes.label), o(input.label)) .
        ^(v("s"), p(prefixes.abbreviation), o(input.abbreviation)).
        ^(v("s"), p(prefixes.id), v("id"))
    }, v("id"))
  }

  override protected def compareModel(input: DegreeProtocol, output: Degree): Boolean = {
    input.label == output.label && input.abbreviation == output.abbreviation
  }

  override protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[Degree]): Try[Set[Degree]] = Success(all)

  override protected def atomize(output: Degree): Try[Option[JsValue]] = Success(Some(Json.toJson(output)))

  override protected def atomizeMany(output: Set[Degree]): Try[JsValue] = Success(Json.toJson(output))

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(degree.get)
    case GetAll => PartialSecureBlock(degree.getAll)
    case _ => PartialSecureBlock(prime)
  }
}