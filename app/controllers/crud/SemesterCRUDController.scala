package controllers.crud

import java.util.UUID

import models.security.{Roles, Role, RefRole}
import models.{Semester, SemesterProtocol, UriGenerator}
import org.joda.time.DateTime
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Json, Reads, Writes}
import play.api.mvc.{Request, Result, AnyContent, Action}
import services.RoleService
import store.{Namespace, SesameRepository}
import utils.LWMActions.{SecureContentTypedAction, SecureAction}
import utils.LwmMimeType
import models.security.Permissions._
import scala.collection.Map
import scala.util.{Failure, Success}
import Roles._

object SemesterCRUDController {
  val yearAttribute = "year"
  val periodAttribute = "period"
}

class SemesterCRUDController(val repository: SesameRepository, val namespace: Namespace, val roleService: RoleService) extends AbstractCRUDController[SemesterProtocol, Semester] {
  override implicit def rdfWrites: ToPG[Sesame, Semester] = defaultBindings.SemesterBinding.semesterBinder

  override implicit def rdfReads: FromPG[Sesame, Semester] = defaultBindings.SemesterBinding.semesterBinder

  override implicit def classUrisFor: ClassUrisFor[Sesame, Semester] = defaultBindings.SemesterBinding.classUri

  override implicit def uriGenerator: UriGenerator[Semester] = Semester

  override implicit def reads: Reads[SemesterProtocol] = Semester.reads

  override implicit def writes: Writes[Semester] = Semester.writes

  override val mimeType: LwmMimeType = LwmMimeType.semesterV1Json

  override def getWithFilter(queryString: Map[String, Seq[String]])(semesters: Set[Semester]) = {
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
              Ok(Json.toJson(filteredByYear)).as(mimeType)
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
                  Ok(Json.toJson(sem)).as(mimeType)
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
                Ok(Json.toJson(sem)).as(mimeType)
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
  }

  override protected def fromInput(input: SemesterProtocol, id: Option[UUID]): Semester = id match {
    case Some(uuid) =>
      Semester(input.name, input.startDate, input.endDate, input.examPeriod, uuid)
    case None =>
      Semester(input.name, input.startDate, input.endDate, input.examPeriod, Semester.randomUUID)
  }

  override protected def invokeAction(act: Rule)(moduleId: Option[String] = None) = Invoke {
    case All => Block(None, Set(allSemesters))
    case Get => Block(None, Set(getSemester))
    case _ => Block((None, admin.permissions))
  }.run(act)


}