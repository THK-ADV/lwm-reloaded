package controllers.crud

import java.util.UUID

import models.{Semester, SemesterProtocol, UriGenerator}
import org.joda.time.DateTime
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Json, Reads, Writes}
import store.{Namespace, SesameRepository}

import scala.collection.Map
import scala.util.{Failure, Success}

object SemesterCRUDController {
  val yearAttribute = "year"
  val periodAttribute = "period"
}

class SemesterCRUDController(val repository: SesameRepository, val namespace: Namespace) extends AbstractCRUDController[SemesterProtocol, Semester] {
  override implicit def rdfWrites: ToPG[Sesame, Semester] = defaultBindings.SemesterBinding.semesterBinder

  override implicit def rdfReads: FromPG[Sesame, Semester] = defaultBindings.SemesterBinding.semesterBinder

  override implicit def classUrisFor: ClassUrisFor[Sesame, Semester] = defaultBindings.SemesterBinding.classUri

  override implicit def uriGenerator: UriGenerator[Semester] = Semester

  override implicit def reads: Reads[SemesterProtocol] = Semester.reads

  override implicit def writes: Writes[Semester] = Semester.writes

  override def getWithFilter(queryString: Map[String, Seq[String]]) = {
    repository.get[Semester] match {
      case Success(semesters) =>
        val attributes = List(queryString.get(SemesterCRUDController.yearAttribute), queryString.get(SemesterCRUDController.periodAttribute))

        attributes match {
          case List(Some(years), None) =>
            val filteredByYear = years.head.split(",").toSet[String].flatMap(year => semesters.filter(sem => DateTime.parse(sem.startDate).getYear.toString.equals(year)))

            if (filteredByYear.isEmpty) {
              NotFound(Json.obj(
                "status" -> "KO",
                "message" -> "No such element..."
              ))
            } else {
              Ok(Json.toJson(filteredByYear))
            }

          case List(Some(years), Some(period)) =>
            val filteredByYear = years.head.split(",").toSet[String].flatMap(year => semesters.filter(sem => DateTime.parse(sem.startDate).getYear.toString.equals(year)))

            if (filteredByYear.isEmpty) {
              NotFound(Json.obj(
                "status" -> "KO",
                "message" -> "No such element..."
              ))
            } else {
              val filteredByPeriod = period.head match {
                case ss if ss.toLowerCase.equals("ss") =>
                  Some(filteredByYear.filter(sem => DateTime.parse(sem.startDate).getMonthOfYear <= 6))
                case ws if ws.toLowerCase.equals("ws") =>
                  Some(filteredByYear.filter(sem => DateTime.parse(sem.startDate).getMonthOfYear > 6))
                case _ => None
              }

              filteredByPeriod match {
                case Some(sem) =>
                  Ok(Json.toJson(sem))
                case None =>
                  NotFound(Json.obj(
                    "status" -> "KO",
                    "message" -> "No such element..."
                  ))
              }
            }

          case List(None, Some(period)) =>
            val filteredByPeriod = period.head match {
              case ss if ss.toLowerCase.equals("ss") =>
                Some(semesters.filter(sem => DateTime.parse(sem.startDate).getMonthOfYear <= 6))
              case ws if ws.toLowerCase.equals("ws") =>
                Some(semesters.filter(sem => DateTime.parse(sem.startDate).getMonthOfYear > 6))
              case _ => None
            }

            filteredByPeriod match {
              case Some(sem) =>
                Ok(Json.toJson(sem))
              case None =>
                NotFound(Json.obj(
                  "status" -> "KO",
                  "message" -> "No such element..."
                ))
            }

          case _ =>
            ServiceUnavailable(Json.obj(
              "status" -> "KO",
              "message" -> "query not found"
            ))
        }

      case Failure(e) =>
        InternalServerError(Json.obj(
          "status" -> "KO",
          "errors" -> e.getMessage
        ))
    }
  }

  override protected def fromInput(input: SemesterProtocol, id: Option[UUID]): Semester = id match {
    case Some(s) =>
      Semester(input.name, input.startDate, input.endDate, input.examPeriod, s)
    case None =>
      Semester(input.name, input.startDate, input.endDate, input.examPeriod, Semester.randomUUID)
  }
}