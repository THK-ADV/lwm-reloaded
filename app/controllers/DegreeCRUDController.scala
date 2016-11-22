package controllers

import models.Permissions._
import models.{Degree, DegreeProtocol, UriGenerator}
import org.w3.banana.RDFPrefix
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Reads, Writes}
import services.{RoleService, SessionHandlingService}
import store.Prefixes.LWMPrefix
import store.bind.Descriptor.Descriptor
import store.sparql.Clause
import store.{Namespace, SesameRepository}
import utils.LwmMimeType

import scala.collection.Map
import scala.util.{Success, Try}

class DegreeCRUDController(val repository: SesameRepository, val sessionService: SessionHandlingService, val namespace: Namespace, val roleService: RoleService) extends AbstractCRUDController[DegreeProtocol, Degree, Degree]{

  override val mimeType: LwmMimeType = LwmMimeType.degreeV1Json

  override implicit val descriptor: Descriptor[Sesame, Degree] = defaultBindings.DegreeDescriptor

  override val descriptorAtom: Descriptor[Sesame, Degree] = descriptor

  override implicit val reads: Reads[DegreeProtocol] = Degree.reads

  override implicit val writes: Writes[Degree] = Degree.writes

  override implicit val writesAtom: Writes[Degree] = Degree.writesAtom

  override implicit val uriGenerator: UriGenerator[Degree] = Degree

  override protected def coAtomic(atom: Degree): Degree = atom

  override protected def compareModel(input: DegreeProtocol, output: Degree): Boolean = input.label == output.label

  override protected def fromInput(input: DegreeProtocol, existing: Option[Degree]): Degree = existing match {
    case Some(degree) => Degree(input.label, input.abbreviation, degree.invalidated, degree.id)
    case None => Degree(input.label, input.abbreviation)
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(degree.get)
    case GetAll => PartialSecureBlock(degree.getAll)
    case Update => PartialSecureBlock(prime)
    case _ => PartialSecureBlock(god)
  }

  override protected def existsQuery(input: DegreeProtocol): Clause = {
    lazy val lwm = LWMPrefix[repository.Rdf]
    lazy val rdf = RDFPrefix[repository.Rdf]

    import store.sparql.select
    import store.sparql.select._

    select ("s") where {
      **(v("s"), p(rdf.`type`), s(lwm.Degree)) .
        **(v("s"), p(lwm.abbreviation), o(input.abbreviation))
    }
  }

  override protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[Degree]): Try[Set[Degree]] = Success(all)

}