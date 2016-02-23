package controllers.crud.semester

import java.util.UUID

import controllers.crud.AbstractCRUDController
import models.UriGenerator
import models.security.Permissions._
import models.semester.{Semester, SemesterProtocol}
import org.w3.banana.RDFPrefix
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Json, Reads, Writes}
import play.api.mvc.Result
import services.RoleService
import store.Prefixes.LWMPrefix
import store.sparql.{select, Clause}
import store.{Namespace, SesameRepository}
import utils.LwmMimeType

import scala.collection.Map

object SemesterCRUDController {
  val yearAttribute = "year"
  val periodAttribute = "period"
}

class SemesterCRUDController(val repository: SesameRepository, val namespace: Namespace, val roleService: RoleService) extends AbstractCRUDController[SemesterProtocol, Semester] {
  override val mimeType: LwmMimeType = LwmMimeType.semesterV1Json

  override implicit def rdfWrites: ToPG[Sesame, Semester] = defaultBindings.SemesterBinding.semesterBinder

  override implicit def rdfReads: FromPG[Sesame, Semester] = defaultBindings.SemesterBinding.semesterBinder

  override implicit def classUrisFor: ClassUrisFor[Sesame, Semester] = defaultBindings.SemesterBinding.classUri

  override implicit def uriGenerator: UriGenerator[Semester] = Semester

  override implicit def reads: Reads[SemesterProtocol] = Semester.reads

  override implicit def writes: Writes[Semester] = Semester.writes

  override def getWithFilter(queryString: Map[String, Seq[String]])(semesters: Set[Semester]): Result = {
    val attributes = List(queryString.get(SemesterCRUDController.yearAttribute), queryString.get(SemesterCRUDController.periodAttribute))

    def filterByYears(years: Seq[String], semesters: Set[Semester]): Set[Semester] = {
      years.head.split(",").toSet[String].flatMap(year => semesters.filter(sem => sem.start.getYear.toString.equals(year)))
    }

    def filterByPeriod(period: Seq[String], semesters: Set[Semester]): Result = {
      val byPeriod = period.head match {
        case ss if ss.toLowerCase.equals("ss") =>
          Some(semesters.filter(sem => sem.start.getMonthOfYear <= 6))
        case ws if ws.toLowerCase.equals("ws") =>
          Some(semesters.filter(sem => sem.start.getMonthOfYear > 6))
        case _ => None
      }

      byPeriod match {
        case Some(sem) =>
          Ok(Json.toJson(sem)).as(mimeType)
        case None =>
          NotFound(Json.obj(
            "status" -> "KO",
            "message" -> "No such element..."
          ))
      }
    }

    attributes match {
      case List(Some(years), None) =>
        val byYears = filterByYears(years, semesters)

        if (byYears.isEmpty) {
          NotFound(Json.obj(
            "status" -> "KO",
            "message" -> "No such element..."
          ))
        } else {
          Ok(Json.toJson(byYears)).as(mimeType)
        }

      case List(Some(years), Some(period)) =>
        val byYears = filterByYears(years, semesters)

        if (byYears.isEmpty) {
          NotFound(Json.obj(
            "status" -> "KO",
            "message" -> "No such element..."
          ))
        } else {
          filterByPeriod(period, byYears)
        }

      case List(None, Some(period)) =>
        filterByPeriod(period, semesters)

      case _ =>
        ServiceUnavailable(Json.obj(
          "status" -> "KO",
          "message" -> "query attribute not found"
        ))
    }
  }

  override protected def fromInput(input: SemesterProtocol, id: Option[UUID]): Semester = id match {
    case Some(uuid) => Semester(input.label, input.abbreviation, input.start, input.end, input.examStart, uuid)
    case None => Semester(input.label, input.abbreviation, input.start, input.end, input.examStart, Semester.randomUUID)
  }


  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case All => PartialSecureBlock(Set(allSemesters))
    case Get => PartialSecureBlock(Set(getSemester))
    case _ => PartialSecureBlock(Set(prime))
  }

  override protected def existsQuery(input: SemesterProtocol): (Clause, select.Var) = {
    lazy val prefixes = LWMPrefix[repository.Rdf]
    lazy val rdf = RDFPrefix[repository.Rdf]
    import store.sparql.select
    import store.sparql.select._

    (select ("id") where {
      ^(v("s"), p(rdf.`type`), s(prefixes.Semester)) .
        ^(v("s"), p(prefixes.label), o(input.label)) .
        ^(v("s"), p(prefixes.start), o(input.start)) .
        ^(v("s"), p(prefixes.end), o(input.end)) .
        ^(v("s"), p(prefixes.id), v("id"))
    }, v("id"))
  }

  override protected def compareModel(input: SemesterProtocol, output: Semester): Boolean = {
    input.label == output.label && input.start.isEqual(output.start) && input.end.isEqual(output.end) && input.examStart.isEqual(output.examStart)
  }
}