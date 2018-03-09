package controllers

import java.util.UUID

import controllers.helper._
import dao._
import models.Role.{Admin, Employee, God, Student}
import play.api.libs.json.{Json, Writes}
import play.api.mvc.{AnyContent, Controller, Request}
import services.SessionHandlingService
import utils.LwmMimeType

import scala.util.Try

final class DashboardController(val authorityDao: AuthorityDao, val sessionService: SessionHandlingService, val dashboardDao: DashboardDao)
  extends Controller
    with Secured
    with SessionChecking
    with ContentTyped
    with SecureControllerContext
    with PostgresResult
    with AttributeFilter {

  implicit val writes: Writes[Dashboard] = new Writes[Dashboard] {
    override def writes(d: Dashboard) = d match {
      case s: StudentDashboard => Json.writes[StudentDashboard].writes(s)
      case e: EmployeeDashboard => Json.writes[EmployeeDashboard].writes(e)
    }
  }

  def dashboard = contextFrom(Get) asyncAction { implicit request =>
    dashboardRequest(None)
  }

  def dashboardFor(user: String) = contextFrom(GetAll) asyncAction { implicit request =>
    dashboardRequest(Try(UUID.fromString(user)).toOption)
  }

  private def dashboardRequest(user: Option[UUID])(implicit request: Request[AnyContent]) = {
    val attr = extractAttributes(request.queryString)._2
    val id = user getOrElse request.session.get(SessionController.userId).map(UUID.fromString).get

    dashboardDao.dashboard(id)(attr.atomic, attr.valid, attr.lastModified).jsonResult
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Get => PartialSecureBlock(List(Student, Employee))
    case GetAll => PartialSecureBlock(List(Admin))
    case _ => PartialSecureBlock(List(God))
  }

  override implicit val mimeType: LwmMimeType = LwmMimeType.json
}
