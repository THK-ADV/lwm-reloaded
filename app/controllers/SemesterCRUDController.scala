package controllers

import models.Permissions._
import models.{SemesterProtocol, SesameSemester, UriGenerator}
import org.w3.banana.RDFPrefix
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Reads, Writes}
import services.{RoleService, RoleServiceLike, SessionHandlingService}
import store.Prefixes.LWMPrefix
import store.bind.Descriptor.Descriptor
import store.sparql.Clause
import store.{Namespace, SesameRepository}
import utils.LwmMimeType

import scala.util.{Failure, Try}

object SemesterCRUDController {
  val selectAttribute = "select"
  val currentValue = "current"
}

class SemesterCRUDController(val repository: SesameRepository, val sessionService: SessionHandlingService, val namespace: Namespace, val roleService: RoleServiceLike) extends AbstractCRUDController[SemesterProtocol, SesameSemester, SesameSemester] {
  override val mimeType: LwmMimeType = LwmMimeType.semesterV1Json

  override implicit def descriptor: Descriptor[Sesame, SesameSemester] = defaultBindings.SemesterDescriptor

  override implicit def uriGenerator: UriGenerator[SesameSemester] = SesameSemester

  override implicit def reads: Reads[SemesterProtocol] = SesameSemester.reads

  override implicit def writes: Writes[SesameSemester] = SesameSemester.writes

  override implicit def writesAtom: Writes[SesameSemester] = SesameSemester.writesAtom

  override protected def fromInput(input: SemesterProtocol, existing: Option[SesameSemester]): SesameSemester = existing match {
    case Some(semester) => SesameSemester(input.label, input.abbreviation, input.start, input.end, input.examStart, semester.invalidated, semester.id)
    case None => SesameSemester(input.label, input.abbreviation, input.start, input.end, input.examStart, None, SesameSemester.randomUUID)
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

  override protected def coAtomic(atom: SesameSemester): SesameSemester = atom

  override implicit def descriptorAtom: Descriptor[Sesame, SesameSemester] = descriptor

  override protected def compareModel(input: SemesterProtocol, output: SesameSemester): Boolean = {
    input.abbreviation == output.abbreviation && input.examStart.isEqual(output.examStart)
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(semester.get)
    case GetAll => PartialSecureBlock(semester.getAll)
    case _ => PartialSecureBlock(prime)
  }

  override protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[SesameSemester]): Try[Set[SesameSemester]] = {
    import models.SesameSemester.isCurrent
    import controllers.SemesterCRUDController._
    queryString.foldLeft(Try(all)) {
      case (set, (`selectAttribute`, current)) if current.head == currentValue => set map (_.filter(isCurrent))
      case (_, (`selectAttribute`, other)) => Failure(new Throwable(s"Value of $selectAttribute should be $currentValue, but was ${other.head}"))
      case _ => Failure(new Throwable("Unknown attribute"))
    }
  }
}