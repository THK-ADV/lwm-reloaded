package controllers

import java.util.UUID

import controllers.helper._
import dao._
import models.Role.{Employee, God, Student}
import play.api.libs.json.Json
import play.api.mvc.Controller
import services.SessionHandlingService
import utils.LwmMimeType

final class DashboardController(val authorityDao: AuthorityDao,
                                val sessionService: SessionHandlingService,
                                val dashboardDao: DashboardDao)
  extends Controller
    with Secured
    with SessionChecking
    with ContentTyped
    with SecureControllerContext
    with PostgresResult {

  def student(student: String, degree: String, semester: String, atomic: String) = contextFrom(Get) asyncAction { _ =>
    dashboardDao.student(UUID.fromString(student), UUID.fromString(degree), UUID.fromString(semester), atomic.toBoolean) jsonResult { res =>
      val (labworks, apps, groups, cards, evals, evalPatterns) = res

      Ok(Json.obj(
        "status" -> "OK",
        "labworks" -> Json.toJson(labworks),
        "labworkApplications" -> Json.toJson(apps),
        "groups" -> Json.toJson(groups),
        "reportCardEntries" -> Json.toJson(cards),
        "reportCardEvaluations" -> Json.toJson(evals),
        "reportCardEvaluationPatterns" -> Json.toJson(evalPatterns)
      ))
    }
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(List(Student))
    case GetAll => PartialSecureBlock(List(Employee))
    case _ => PartialSecureBlock(List(God))
  }

  override implicit val mimeType: LwmMimeType = LwmMimeType.json
}
