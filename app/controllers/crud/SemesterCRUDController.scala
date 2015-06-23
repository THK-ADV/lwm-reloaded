package controllers.crud

import models.{Semester, SemesterProtocol, UriGenerator}
import org.joda.time.DateTime
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Json, Reads, Writes}
import store.{Namespace, SesameRepository}

import scala.collection.Map
import scala.util.{Failure, Success}

class SemesterCRUDController(val repository: SesameRepository, val namespace: Namespace) extends AbstractCRUDController2[SemesterProtocol, Semester] {
  override implicit def rdfWrites: ToPG[Sesame, Semester] = defaultBindings.SemesterBinding.semesterBinder

  override implicit def rdfReads: FromPG[Sesame, Semester] = defaultBindings.SemesterBinding.semesterBinder

  override implicit def classUrisFor: ClassUrisFor[Sesame, Semester] = defaultBindings.SemesterBinding.classUri

  override implicit def uriGenerator: UriGenerator[Semester] = Semester

  override implicit def reads: Reads[SemesterProtocol] = Semester.reads

  override implicit def writes: Writes[Semester] = Semester.writes

  override def getWithFilter(queryString: Map[String, Seq[String]]) = {
    repository.get[Semester] match {
      case Success(semesters) =>
        val attributes = List(queryString.get("year"), queryString.get("period"))

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
                case ss if ss.equals("SS") =>
                  Some(semesters.filter(sem => DateTime.parse(sem.startDate).getMonthOfYear <= 6))
                case ws if ws.equals("WS") =>
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
            }

          case List(None, Some(period)) =>
            val filteredByPeriod = period.head match {
              case ss if ss.equals("SS") =>
                Some(semesters.filter(sem => DateTime.parse(sem.startDate).getMonthOfYear <= 6))
              case ws if ws.equals("WS") =>
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

  override protected def fromInput(input: SemesterProtocol): Semester = Semester(input.name, input.startDate, input.endDate, input.examPeriod, Some(Semester.randomUUID))
}
