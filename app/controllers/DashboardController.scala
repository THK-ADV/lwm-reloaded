package controllers

import controllers.helper._
import dao._
import javax.inject.{Inject, Singleton}
import models.Role.{EmployeeRole, God, StudentRole}
import models.{Dashboard, EmployeeDashboard, StudentDashboard}
import play.api.libs.json.{Json, Writes}
import play.api.mvc._
import security.SecurityActionChain

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object DashboardController {
  lazy val systemIdAttribute = "systemId"
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

  implicit val writes: Writes[Dashboard] = {
    case s: StudentDashboard => Json.writes[StudentDashboard].writes(s)
    case e: EmployeeDashboard => Json.writes[EmployeeDashboard].writes(e)
  }

  def dashboard = contextFrom(Get) asyncAction { implicit request =>
    (for {
      id <- Future.fromTry(extractSystemId)
      atomic = extractAttributes(request.queryString)._2.atomic
      board <- dashboardDao.dashboard(id)(atomic)
    } yield board).created
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
