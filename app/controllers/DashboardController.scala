package controllers

import controllers.helper._
import dao._
import database.helper.LdapUserStatus
import javax.inject.{Inject, Singleton}
import models.Role.{EmployeeRole, God, StudentRole}
import models.{Dashboard, EmployeeDashboard, StudentDashboard}
import play.api.libs.json.{JsString, Json, Writes}
import play.api.mvc._
import security.SecurityActionChain

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object DashboardController {
  lazy val systemIdAttribute = "systemId"
  lazy val numberOfUpcomingElements = "numberOfUpcomingElements"
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
    case s: StudentDashboard => Json.writes[StudentDashboard].writes(s)
    case e: EmployeeDashboard => Json.writes[EmployeeDashboard].writes(e)
  }

  implicit val statusWrites: Writes[LdapUserStatus] = (o: LdapUserStatus) => JsString(o.label)

  def dashboard = contextFrom(Get) asyncAction { implicit request =>
    (for {
      id <- Future.fromTry(extractSystemId)
      atomic = extractAttributes(request.queryString)._2.atomic
      numberOfUpcomingElements = intOf(request.queryString)(DashboardController.numberOfUpcomingElements)
      board <- dashboardDao.dashboard(id)(atomic, numberOfUpcomingElements)
    } yield board).jsonResult(d => Ok(Json.toJson(d)))
  }

  private def extractSystemId(implicit request: Request[AnyContent]): Try[String] = {
    import utils.Ops.OptionOps

    val explicitly = valueOf(request.queryString)(DashboardController.systemIdAttribute)
    val implicitly = request.systemId

    explicitly orElse implicitly toTry new Throwable("No User ID found in request")
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(List(StudentRole, EmployeeRole))
    case _ => PartialSecureBlock(List(God))
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case _ => PartialSecureBlock(List(God))
  }
}
