package controllers.crud.labwork

import java.net.URLDecoder
import java.util.UUID

import controllers.crud.AbstractCRUDController
import controllers.crud.labwork.LabworkApplicationCRUDController._
import models.UriGenerator
import models.labwork.{Labwork, LabworkApplication, LabworkApplicationAtom, LabworkApplicationProtocol}
import models.security.Permissions._
import models.users.User
import org.joda.time.DateTime
import org.w3.banana.RDFPrefix
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Reads, Writes}
import services.{RoleService, SessionHandlingService}
import store.Prefixes.LWMPrefix
import store.bind.Descriptor.Descriptor
import store.{Namespace, SesameRepository}
import utils.LwmMimeType
import store.sparql.{Clause, select}

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
class LabworkApplicationCRUDController(val repository: SesameRepository, val sessionService: SessionHandlingService, val namespace: Namespace, val roleService: RoleService) extends AbstractCRUDController[LabworkApplicationProtocol, LabworkApplication, LabworkApplicationAtom] {

  override implicit def reads: Reads[LabworkApplicationProtocol] = LabworkApplication.reads

  override implicit def writes: Writes[LabworkApplication] = LabworkApplication.writes

  override implicit def writesAtom: Writes[LabworkApplicationAtom] = LabworkApplication.writesAtom

  override protected def fromInput(input: LabworkApplicationProtocol, existing: Option[LabworkApplication]): LabworkApplication = existing match {
    case Some(lapp) => LabworkApplication(input.labwork, input.applicant, input.friends, lapp.timestamp, lapp.id)
    case None => LabworkApplication(input.labwork, input.applicant, input.friends)
  }

  override implicit val mimeType: LwmMimeType = LwmMimeType.labworkApplicationV1Json

  override implicit def uriGenerator: UriGenerator[LabworkApplication] = LabworkApplication

  override implicit def descriptor: Descriptor[Sesame, LabworkApplication] = defaultBindings.LabworkApplicationDescriptor

  override protected def compareModel(input: LabworkApplicationProtocol, output: LabworkApplication): Boolean = input.friends == output.friends

  override protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[LabworkApplication]): Try[Set[LabworkApplication]] = {
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

  override protected def coatomic(atom: LabworkApplicationAtom): LabworkApplication = LabworkApplication(atom.labwork.id, atom.applicant.id, atom.friends map (_.id), atom.timestamp, atom.id)

  override implicit def descriptorAtom: Descriptor[Sesame, LabworkApplicationAtom] = defaultBindings.LabworkApplicationAtomDescriptor

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Create => PartialSecureBlock(labworkApplication.create)
    case Update => PartialSecureBlock(labworkApplication.update)
    case Delete => PartialSecureBlock(labworkApplication.delete)
    case Get => PartialSecureBlock(labworkApplication.get)
    case GetAll => PartialSecureBlock(labworkApplication.getAll)
  }

  override protected def existsQuery(input: LabworkApplicationProtocol): (Clause, select.Var) = {
    import store.sparql.select._

    lazy val prefixes = LWMPrefix[repository.Rdf]
    lazy val rdf = RDFPrefix[repository.Rdf]

    (select("id") where {
      **(v("s"), p(rdf.`type`), s(prefixes.LabworkApplication)).
        **(v("s"), p(prefixes.labwork), s(Labwork.generateUri(input.labwork)(namespace))).
        **(v("s"), p(prefixes.applicant), s(User.generateUri(input.applicant)(namespace))).
        **(v("s"), p(prefixes.id), v("id"))
    }, v("id"))
  }
}
