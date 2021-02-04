package controllers

import controllers.helper.{RequestOps, ResultOps, SecureControllerContext, Secured}
import dao.AuthorityDao
import models.{CourseLike, LabworkAtom}
import play.api.libs.json.Json
import play.api.mvc.{AbstractController, ControllerComponents}
import security.LWMRole.{EmployeeRole, StudentRole}
import security.SecurityActionChain
import service.StudentSearchService

import java.util.UUID
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

@Singleton
final class StudentSearchController @Inject()(
  cc: ControllerComponents,
  val securedAction: SecurityActionChain,
  val authorityDao: AuthorityDao,
  val service: StudentSearchService,
  implicit val ctx: ExecutionContext
) extends AbstractController(cc)
  with Secured
  with SecureControllerContext
  with ResultOps
  with RequestOps {

  def dashboard(student: String) = contextFrom(GetAll) asyncAction { request =>
    import utils.Ops.OptionOps

    (for {
      systemId <- Future.fromTry(request.systemId.toTry("No User ID found in request"))
      studentId <- Future.fromTry(Try(UUID.fromString(student)))
      board <- service.dashboard(systemId, studentId)
    } yield board).jsonResult {
      case (user, semester, reportCards) => Ok(Json.obj(
        "user" -> user,
        "semester" -> semester,
        "courses" -> reportCards.map {
          case (course, content) => Json.obj(
            "course" -> Json.toJson(course)(CourseLike.writes),
            "labworks" -> content.map {
              case (labwork, reportCardEntryData, evals) => Json.obj(
                "labwork" -> Json.toJson(labwork)(LabworkAtom.writes),
                "reportCardEntries" -> Json.toJson(reportCardEntryData),
                "evals" -> evals
              )
            }
          )
        }
      ))
    }
  }

  override protected def restrictedContext(restrictionId: String) =
    forbiddenAction()

  override protected def contextFrom = {
    case GetAll => PartialSecureBlock(List(StudentRole, EmployeeRole))
  }
}
