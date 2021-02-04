package controllers

import controllers.helper._
import dao._
import database.helper.LdapUserStatus
import javax.inject.{Inject, Singleton}
import security.LWMRole.{EmployeeRole, God, StudentRole}
import models.{CourseAtom, Dashboard, EmployeeDashboard, StudentDashboard}
import play.api.libs.json.{JsString, Json, Writes}
import play.api.mvc._
import security.SecurityActionChain
import service.dashboard.{DashboardConfig, DashboardService}

import scala.concurrent.{ExecutionContext, Future}

object DashboardController {
  lazy val numberOfUpcomingElementsAttribute = "numberOfUpcomingElements"
  lazy val entriesSinceNowAttribute = "entriesSinceNow"
  lazy val sortedByDateAttribute = "sortedByDate"
  lazy val ownEntriesOnlyAttribute = "ownEntriesOnly"
}

@Singleton
final class DashboardController @Inject()(
  cc: ControllerComponents,
  val authorityDao: AuthorityDao,
  val service: DashboardService,
  val securedAction: SecurityActionChain
)(implicit executionContext: ExecutionContext)
  extends AbstractController(cc)
    with Secured
    with SecureControllerContext
    with ResultOps
    with AttributeFilter
    with RequestOps {

  implicit val dashboardWrites: Writes[Dashboard] = {
    implicit def ldapStatusWrites: Writes[LdapUserStatus] =
      (o: LdapUserStatus) => JsString(o.label)

    {
      case s: StudentDashboard =>
        Json.writes[StudentDashboard].writes(s)
      case e: EmployeeDashboard =>
        implicit def courseWrites: Writes[CourseAtom] = models.CourseAtom.writes

        Json.writes[EmployeeDashboard].writes(e)
    }
  }

  def dashboard = contextFrom(Get) asyncAction { request =>
    import utils.Ops.OptionOps

    (for {
      id <- Future.fromTry(request.systemId.toTry("No User ID found in request"))
      config = parseConfig(request)
      board <- service.dashboard(id, config)
    } yield board).jsonResult
  }

  private def parseConfig(request: Request[AnyContent]): DashboardConfig = {
    import controllers.DashboardController._

    DashboardConfig(
      extractAttributes(request.queryString)._2.atomic,
      intOf(request.queryString)(numberOfUpcomingElementsAttribute),
      boolOf(request.queryString)(entriesSinceNowAttribute),
      boolOf(request.queryString)(sortedByDateAttribute),
      boolOf(request.queryString)(ownEntriesOnlyAttribute),
    )
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(List(StudentRole, EmployeeRole))
    case _ => PartialSecureBlock(List(God))
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = forbiddenAction()
}
