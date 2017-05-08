package controllers

import java.net.URLDecoder
import java.util.UUID

import models.Permissions.labworkApplication
import models._
import org.joda.time.DateTime
import org.w3.banana.RDFPrefix
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Reads, Writes}
import services.{RoleServiceLike, SessionHandlingService}
import store.Prefixes.LWMPrefix
import store.bind.Descriptor.Descriptor
import store.sparql.{Clause, select}
import store.{Namespace, SesameRepository}
import utils.LwmMimeType
import controllers.LabworkApplicationCRUDController._

import scala.collection.Map
import scala.util.{Failure, Try}

object LabworkApplicationCRUDController {
  val labworkAttribute = "labwork"
  val applicantAttribute = "applicant"
  val friendAttribute = "friend"
  val dateAttribute = "date"
  val minTime = "minTime"
  val maxTime = "maxTime"
}

//DateTime format is: yyyy-MM-dd'T'HH:mm
class LabworkApplicationCRUDController(val repository: SesameRepository, val sessionService: SessionHandlingService, val namespace: Namespace, val roleService: RoleServiceLike) extends AbstractCRUDController[SesameLabworkApplicationProtocol, SesameLabworkApplication, SesameLabworkApplicationAtom] {

  override implicit val mimeType: LwmMimeType = LwmMimeType.labworkApplicationV1Json

  override implicit val descriptor: Descriptor[Sesame, SesameLabworkApplication] = defaultBindings.LabworkApplicationDescriptor

  override implicit val descriptorAtom: Descriptor[Sesame, SesameLabworkApplicationAtom] = defaultBindings.LabworkApplicationAtomDescriptor

  override implicit val reads: Reads[SesameLabworkApplicationProtocol] = SesameLabworkApplication.reads

  override implicit val writes: Writes[SesameLabworkApplication] = SesameLabworkApplication.writes

  override implicit val writesAtom: Writes[SesameLabworkApplicationAtom] = SesameLabworkApplication.writesAtom

  override implicit val uriGenerator: UriGenerator[SesameLabworkApplication] = SesameLabworkApplication

  override protected def coAtomic(atom: SesameLabworkApplicationAtom): SesameLabworkApplication = SesameLabworkApplication(atom.labwork.id, atom.applicant.id, atom.friends map (_.id), atom.timestamp, atom.invalidated, atom.id)

  override protected def compareModel(input: SesameLabworkApplicationProtocol, output: SesameLabworkApplication): Boolean = input.friends == output.friends

  override protected def fromInput(input: SesameLabworkApplicationProtocol, existing: Option[SesameLabworkApplication]): SesameLabworkApplication = existing match {
    case Some(lapp) => SesameLabworkApplication(input.labwork, input.applicant, input.friends, lapp.timestamp, lapp.invalidated, lapp.id)
    case None => SesameLabworkApplication(input.labwork, input.applicant, input.friends)
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Create => PartialSecureBlock(labworkApplication.create)
    case Update => PartialSecureBlock(labworkApplication.update)
    case Delete => PartialSecureBlock(labworkApplication.delete)
    case Get => PartialSecureBlock(labworkApplication.get)
    case GetAll => PartialSecureBlock(labworkApplication.getAll)
  }

  override protected def existsQuery(input: SesameLabworkApplicationProtocol): Clause = {
    import store.sparql.select._

    lazy val prefixes = LWMPrefix[repository.Rdf]
    lazy val rdf = RDFPrefix[repository.Rdf]

    select("s") where {
      **(v("s"), p(rdf.`type`), s(prefixes.LabworkApplication)).
        **(v("s"), p(prefixes.labwork), s(SesameLabwork.generateUri(input.labwork)(namespace))).
        **(v("s"), p(prefixes.applicant), s(User.generateUri(input.applicant)(namespace)))
    }
  }

  override protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[SesameLabworkApplication]): Try[Set[SesameLabworkApplication]] = {
    def decode(s: String) = URLDecoder.decode(s, "UTF-8")

    queryString.foldRight(Try(all)) {
      case ((`labworkAttribute`, v), t) => t flatMap (set => Try(UUID.fromString(v.head)).map(v => set.filter(_.labwork == v)))
      case ((`applicantAttribute`, v), t) => t flatMap (set => Try(UUID.fromString(v.head)).map(v => set.filter(_.applicant == v)))
      case ((`friendAttribute`, v), t) => t flatMap (set => Try(UUID.fromString(v.head)).map(v => set.filter(_.friends.exists(_ == v))))
      case ((`dateAttribute`, v), set) => set map (_.filter(_.timestamp.toString("yyyy-MM-dd") == v.head))
      case ((`minTime`, v), t) => t flatMap (set => Try(DateTime.parse(decode(v.head))).map(t => set.filter(_.timestamp.isAfter(t))))
      case ((`maxTime`, v), t) => t flatMap (set => Try(DateTime.parse(decode(v.head))).map(t => set.filter(_.timestamp.isBefore(t))))
      case ((_, _), t) => Failure(new Throwable("Unknown attribute"))
    }
  }
}
