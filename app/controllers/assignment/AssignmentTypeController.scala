package controllers.assignment

import controllers.helper.{ResultOps, SecureControllerContext}
import javax.inject.{Inject, Singleton}
import models.Role.{CourseEmployee, CourseManager, God}
import models.assignment.AssignmentType
import play.api.libs.json.{JsValue, Json}
import play.api.mvc.{AbstractController, Action, AnyContent, ControllerComponents}
import security.SecurityActionChain

@Singleton
final class AssignmentTypeController @Inject()(
  cc: ControllerComponents,
  val securedAction: SecurityActionChain
) extends AbstractController(cc)
  with SecureControllerContext
  with ResultOps {

  def all(course: String): Action[AnyContent] = restrictedContext(course)(GetAll) action { _ =>
    import AssignmentType._

    def displayName(t: AssignmentType): String = t match {
      case Attendance => "Anwesenheitspflichtig"
      case Bonus => "Bonus"
      case SimpleCertificate => "Einfaches Zertifikat"
      case DetailedCertificate => "Detailiertes Zertifikat"
      case NumberedCertificate => "Punktebasiertes Zertificat"
    }

    def makeJson(t: AssignmentType): JsValue =
      Json.obj("identifier" -> identifier(t), "label" -> displayName(t))

    ok(AssignmentType.types().map(makeJson)) // TODO maybe add categories like optional -> bonus, cert -> A, B, C
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case GetAll => SecureBlock(restrictionId, List(CourseManager, CourseEmployee))
    case _ => PartialSecureBlock(List(God))
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = forbiddenAction()
}
