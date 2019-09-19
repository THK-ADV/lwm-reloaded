package controllers

import controllers.helper._
import dao._
import database.helper.LdapUserStatus
import javax.inject.{Inject, Singleton}
import models.Role.{EmployeeRole, God, StudentRole}
import models.{Dashboard, EmployeeDashboard, LabworkApplicationLike, LabworkLike, ReportCardEntryLike, ReportCardEvaluationLike, Semester, StudentDashboard, User}
import play.api.libs.json.{JsString, Json, Writes}
import play.api.mvc._
import security.SecurityActionChain

import scala.concurrent.{ExecutionContext, Future}

object DashboardController {
  lazy val numberOfUpcomingElementsAttribute = "numberOfUpcomingElements"
  lazy val entriesSinceNowAttribute = "entriesSinceNow"
  lazy val sortedByDateAttribute = "sortedByDate"
}

@Singleton
final class DashboardController @Inject()(
  cc: ControllerComponents,
  val authorityDao: AuthorityDao,
  val dashboardDao: DashboardDao,
  val securedAction: SecurityActionChain
)(implicit executionContext: ExecutionContext)
  extends AbstractController(cc)
    with Secured
    with SecureControllerContext
    with ResultOps
    with AttributeFilter
    with RequestOps {

  implicit val dashboardWrites: Writes[Dashboard] = {
    implicit val ldapStatusWrites: Writes[LdapUserStatus] = (o: LdapUserStatus) => JsString(o.label)

    {
      case s: StudentDashboard => Json.obj(
        "user" -> User.writes.writes(s.user),
        "status" -> s.status,
        "semester" -> Semester.writes.writes(s.semester),
        "labworks" -> s.labworks.map(LabworkLike.writes.writes),
        "labworkApplications" -> s.labworkApplications.map(LabworkApplicationLike.writes.writes),
        "groups" -> s.groups.map {
          case (groupLabel, labwork) => Json.obj(
            "groupLabel" -> groupLabel,
            "labwork" -> LabworkLike.writes.writes(labwork)
          )
        },
        "reportCardEntries" -> s.reportCardEntries.map(ReportCardEntryLike.writes.writes),
        "allEvaluations" -> s.allEvaluations.map(ReportCardEvaluationLike.writes.writes),
        "passedEvaluations" -> s.passedEvaluations.map {
          case (course, semester, passed, bonus) => Json.obj(
            "course" -> course,
            "semester" -> semester,
            "passed" -> passed,
            "bonus" -> bonus
          )
        }
      )
      case e: EmployeeDashboard => {
        import models.CourseAtom.writes
        Json.writes[EmployeeDashboard].writes(e)
      }
    }
  }

  def dashboard(systemId: Option[String]) = contextFrom(Get) asyncAction { implicit request =>
    import DashboardController._
    import utils.Ops.OptionOps

    val explicitly = systemId
    val implicitly = request.systemId

    (for {
      id <- Future.fromTry(explicitly orElse implicitly toTry new Throwable("No User ID found in request"))
      atomic = extractAttributes(request.queryString)._2.atomic
      numberOfUpcomingElements = intOf(request.queryString)(numberOfUpcomingElementsAttribute)
      entriesSinceNow = boolOf(request.queryString)(entriesSinceNowAttribute) getOrElse true
      sortedByDate = boolOf(request.queryString)(sortedByDateAttribute) getOrElse true
      board <- dashboardDao.dashboard(id)(atomic, numberOfUpcomingElements, entriesSinceNow, sortedByDate)
    } yield board).jsonResult
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(List(StudentRole, EmployeeRole))
    case _ => PartialSecureBlock(List(God))
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case _ => PartialSecureBlock(List(God))
  }
}
