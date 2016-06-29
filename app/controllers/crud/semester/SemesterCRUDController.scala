package controllers.crud.semester

import controllers.crud.AbstractCRUDController
import models.UriGenerator
import models.security.Permissions._
import models.semester.{Semester, SemesterProtocol}
import org.w3.banana.RDFPrefix
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{JsValue, Json, Reads, Writes}
import services.{RoleService, SessionHandlingService}
import store.Prefixes.LWMPrefix
import store.bind.Descriptor.{CompositeClassUris, Descriptor}
import store.sparql.{Clause, select}
import store.{Namespace, SesameRepository}
import utils.LwmMimeType

import scala.collection.Map
import scala.util.{Failure, Success, Try}

object SemesterCRUDController {
  val yearAttribute = "year"
  val periodAttribute = "period"
}

class SemesterCRUDController(val repository: SesameRepository, val sessionService: SessionHandlingService, val namespace: Namespace, val roleService: RoleService) extends AbstractCRUDController[SemesterProtocol, Semester, Semester] {
  override val mimeType: LwmMimeType = LwmMimeType.semesterV1Json

  override implicit def descriptor: Descriptor[Sesame, Semester] = defaultBindings.SemesterDescriptor

  override implicit def uriGenerator: UriGenerator[Semester] = Semester

  override implicit def reads: Reads[SemesterProtocol] = Semester.reads

  override implicit def writes: Writes[Semester] = Semester.writes

  override implicit def writesAtom: Writes[Semester] = Semester.writesAtom

  override protected def fromInput(input: SemesterProtocol, existing: Option[Semester]): Semester = existing match {
    case Some(semester) => Semester(input.label, input.abbreviation, input.start, input.end, input.examStart, semester.id)
    case None => Semester(input.label, input.abbreviation, input.start, input.end, input.examStart, Semester.randomUUID)
  }

  override protected def existsQuery(input: SemesterProtocol): (Clause, select.Var) = {
    lazy val prefixes = LWMPrefix[repository.Rdf]
    lazy val rdf = RDFPrefix[repository.Rdf]
    import store.sparql.select
    import store.sparql.select._

    (select ("id") where {
      **(v("s"), p(rdf.`type`), s(prefixes.Semester)) .
        **(v("s"), p(prefixes.label), o(input.label)) .
        **(v("s"), p(prefixes.start), o(input.start)) .
        **(v("s"), p(prefixes.end), o(input.end)) .
        **(v("s"), p(prefixes.id), v("id"))
    }, v("id"))
  }

  override protected def coatomic(atom: Semester): Semester = atom

  override implicit def descriptorAtom: Descriptor[Sesame, Semester] = descriptor

  override protected def compareModel(input: SemesterProtocol, output: Semester): Boolean = {
    input.abbreviation == output.abbreviation && input.examStart.isEqual(output.examStart)
  }

  override protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[Semester]): Try[Set[Semester]] = {
    val attributes = List(queryString.get(SemesterCRUDController.yearAttribute), queryString.get(SemesterCRUDController.periodAttribute))

    def filterByYears(years: Seq[String], semesters: Set[Semester]): Set[Semester] = {
      years.head.split(",").toSet[String].flatMap(year => semesters.filter(sem => sem.start.getYear.toString.equals(year)))
    }

    def filterByPeriod(period: Seq[String], semesters: Set[Semester]): Try[Set[Semester]] = {
      period.head match {
        case ss if ss.toLowerCase.equals("ss") =>
          Success(semesters.filter(sem => sem.start.getMonthOfYear <= 6))
        case ws if ws.toLowerCase.equals("ws") =>
          Success(semesters.filter(sem => sem.start.getMonthOfYear > 6))
        case _ => Failure(new Throwable("Unknown attribute"))
      }
    }

    attributes match {
      case List(Some(years), None) => Success(filterByYears(years, all))
      case List(Some(years), Some(period)) => filterByPeriod(period, filterByYears(years, all))
      case List(None, Some(period)) => filterByPeriod(period, all)
      case _ => Failure(new Throwable("Unknown attribute"))
    }
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(semester.get)
    case GetAll => PartialSecureBlock(semester.getAll)
    case _ => PartialSecureBlock(prime)
  }
}