package controllers

import controllers.helper._
import dao._
import javax.inject.{Inject, Singleton}
import models.Role.{Admin, EmployeeRole, God, StudentRole}
import models.{Dashboard, EmployeeDashboard, StudentDashboard}
import play.api.libs.json.{Json, Writes}
import play.api.mvc._
import utils.SecuredAction

import scala.concurrent.Future

@Singleton
final class DashboardController @Inject()(cc: ControllerComponents, val authorityDao: AuthorityDao, val dashboardDao: DashboardDao, val securedAction: SecuredAction)
  extends AbstractController(cc)
    with Secured
    with SecureControllerContext
    with ResultOps
    with AttributeFilter
    with RequestOps {

  implicit val writes: Writes[Dashboard] = {
    case s: StudentDashboard => Json.writes[StudentDashboard].writes(s)
    case e: EmployeeDashboard => Json.writes[EmployeeDashboard].writes(e)
  }

  def dashboard = contextFrom(Get) asyncAction { implicit request =>
    import utils.Ops.OptionOps
    import scala.concurrent.ExecutionContext.Implicits.global

    (for {
      systemId <- Future.fromTry(request.systemId.toTry(new Throwable("No User ID found in request")))
      attr = extractAttributes(request.queryString)._2
      board <- dashboardDao.dashboard(systemId)(attr.atomic, attr.valid, attr.lastModified)
    } yield board).jsonResult
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(List(StudentRole, EmployeeRole))
    case GetAll => PartialSecureBlock(List(Admin))
    case _ => PartialSecureBlock(List(God))
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case _ => PartialSecureBlock(List(God))
  }
}
