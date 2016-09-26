package controllers.crud.semester

import controllers.crud.AbstractCRUDController
import models.UriGenerator
import models.security.Permissions._
import models.semester.{Semester, SemesterProtocol}
import org.w3.banana.RDFPrefix
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Reads, Writes}
import services.{RoleService, SessionHandlingService}
import store.Prefixes.LWMPrefix
import store.bind.Descriptor.Descriptor
import store.sparql.{Clause, select}
import store.{Namespace, SesameRepository}
import utils.LwmMimeType

import scala.collection.Map
import scala.util.{Failure, Try}

object SemesterCRUDController {
  val selectAttribute = "select"
  val currentValue = "current"
}

class SemesterCRUDController(val repository: SesameRepository, val sessionService: SessionHandlingService, val namespace: Namespace, val roleService: RoleService) extends AbstractCRUDController[SemesterProtocol, Semester, Semester] {
  override val mimeType: LwmMimeType = LwmMimeType.semesterV1Json

  override implicit def descriptor: Descriptor[Sesame, Semester] = defaultBindings.SemesterDescriptor

  override implicit def uriGenerator: UriGenerator[Semester] = Semester

  override implicit def reads: Reads[SemesterProtocol] = Semester.reads

  override implicit def writes: Writes[Semester] = Semester.writes

  override implicit def writesAtom: Writes[Semester] = Semester.writesAtom

  override protected def fromInput(input: SemesterProtocol, existing: Option[Semester]): Semester = existing match {
    case Some(semester) => Semester(input.label, input.abbreviation, input.start, input.end, input.examStart, semester.invalidated, semester.id)
    case None => Semester(input.label, input.abbreviation, input.start, input.end, input.examStart, None, Semester.randomUUID)
  }

  override protected def existsQuery(input: SemesterProtocol): Clause = {
    import store.sparql.select
    import store.sparql.select._

    lazy val lwm = LWMPrefix[repository.Rdf]
    lazy val rdf = RDFPrefix[repository.Rdf]

    select ("id") where {
      **(v("s"), p(rdf.`type`), s(lwm.Semester)) .
        **(v("s"), p(lwm.label), o(input.label)) .
        **(v("s"), p(lwm.start), o(input.start)) .
        **(v("s"), p(lwm.end), o(input.end))
    }
  }

  override protected def coAtomic(atom: Semester): Semester = atom

  override implicit def descriptorAtom: Descriptor[Sesame, Semester] = descriptor

  override protected def compareModel(input: SemesterProtocol, output: Semester): Boolean = {
    input.abbreviation == output.abbreviation && input.examStart.isEqual(output.examStart)
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(semester.get)
    case GetAll => PartialSecureBlock(semester.getAll)
    case _ => PartialSecureBlock(prime)
  }

  override protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[Semester]): Try[Set[Semester]] = {
    import controllers.crud.semester.SemesterCRUDController._
    import models.semester.Semester.currentPredicate

    queryString.foldLeft(Try(all)) {
      case (set, (`selectAttribute`, current)) if current.head == currentValue => set map (_.filter(currentPredicate))
      case (_, (`selectAttribute`, other)) => Failure(new Throwable(s"Value of $selectAttribute should be $currentValue, but was ${other.head}"))
      case _ => Failure(new Throwable("Unknown attribute"))
    }
  }
}