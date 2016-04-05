package controllers.crud.labwork

import java.net.URLDecoder
import java.util.UUID

import controllers.crud.AbstractCRUDController
import controllers.crud.labwork.LabworkApplicationCRUDController._
import models.UriGenerator
import models.labwork.{Labwork, LabworkApplication, LabworkApplicationAtom, LabworkApplicationProtocol}
import models.security.Permissions._
import models.users.{Student, User}
import org.joda.time.DateTime
import org.w3.banana.RDFPrefix
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{JsValue, Json, Reads, Writes}
import services.{RoleService, SessionHandlingService}
import store.Prefixes.LWMPrefix
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
class LabworkApplicationCRUDController(val repository: SesameRepository, val sessionService: SessionHandlingService, val namespace: Namespace, val roleService: RoleService) extends AbstractCRUDController[LabworkApplicationProtocol, LabworkApplication] {

  override implicit def reads: Reads[LabworkApplicationProtocol] = LabworkApplication.reads

  override implicit def writes: Writes[LabworkApplication] = LabworkApplication.writes

  override protected def fromInput(input: LabworkApplicationProtocol, existing: Option[LabworkApplication]): LabworkApplication = existing match {
    case Some(lapp) => LabworkApplication(input.labwork, input.applicant, input.friends, lapp.timestamp, lapp.id)
    case None => LabworkApplication(input.labwork, input.applicant, input.friends)
  }

  override implicit val mimeType: LwmMimeType = LwmMimeType.labworkApplicationV1Json

  override implicit def rdfReads: FromPG[Sesame, LabworkApplication] = defaultBindings.LabworkApplicationBinding.labworkApplicationBinder

  override implicit def classUrisFor: ClassUrisFor[Sesame, LabworkApplication] = defaultBindings.LabworkApplicationBinding.classUri

  override implicit def uriGenerator: UriGenerator[LabworkApplication] = LabworkApplication

  override implicit def rdfWrites: ToPG[Sesame, LabworkApplication] = defaultBindings.LabworkApplicationBinding.labworkApplicationBinder

  override protected def compareModel(input: LabworkApplicationProtocol, output: LabworkApplication): Boolean = input.friends == output.friends

  override protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[LabworkApplication]): Try[Set[LabworkApplication]] = {
    def decode(s: String) = URLDecoder.decode(s, "UTF-8")

    queryString.foldRight(Try(all)) {
      case ((`labworkAttribute`, v), t) => t flatMap (set => Try(UUID.fromString(v.head)).map(v => set.filter(_.labwork == v)))
      case ((`applicantAttribute`, v), t) => t flatMap (set => Try(UUID.fromString(v.head)).map(v => set.filter(_.applicant == v)))
      case ((`friendAttribute`, v), t) => t flatMap (set => Try(UUID.fromString(v.head)).map(v => set.filter (_.friends.exists(_ == v))))
      case ((`dateAttribute`, v), set) => set map (_.filter (_.timestamp.toString("yyyy-MM-dd") == v.head))
      case ((`minTime`, v), t) => t flatMap (set => Try(DateTime.parse(decode(v.head))).map(t => set.filter(_.timestamp.isAfter(t))))
      case ((`maxTime`, v), t) => t flatMap (set => Try(DateTime.parse(decode(v.head))).map(t => set.filter(_.timestamp.isBefore(t))))
      case ((_, _), t) => Failure(new Throwable("Unknown attribute"))
    }
  }

  override protected def atomize(output: LabworkApplication): Try[Option[JsValue]] = {
    import LabworkApplication.atomicWrites
    import defaultBindings.LabworkBinding.labworkBinder
    import defaultBindings.StudentBinding.studentBinder

    for {
      labwork <- repository.get[Labwork](Labwork.generateUri(output.labwork)(namespace))
      applicant <- repository.get[Student](User.generateUri(output.applicant)(namespace))
      friends <- repository.getMany[Student](output.friends.map(id => User.generateUri(id)(namespace)))
    } yield for {
      l <- labwork; app <- applicant
    } yield Json.toJson(
      LabworkApplicationAtom(l, app, friends, output.timestamp, output.id)
    )
  }

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

    (select ("id") where {
      ^(v("s"), p(rdf.`type`), s(prefixes.LabworkApplication)) .
        ^(v("s"), p(prefixes.labwork), s(Labwork.generateUri(input.labwork)(namespace))) .
        ^(v("s"), p(prefixes.applicant), s(User.generateUri(input.applicant)(namespace))) .
        ^(v("s"), p(prefixes.id), v("id"))
    }, v("id"))
  }
}
