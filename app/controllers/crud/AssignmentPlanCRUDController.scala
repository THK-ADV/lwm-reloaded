package controllers.crud

import java.util.UUID

import models._
import models.security.Permissions
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{JsValue, Json, Reads, Writes}
import services.{RoleService, SessionHandlingService}
import store.{Namespace, SesameRepository}
import utils.LwmMimeType

import scala.collection.Map
import scala.util.{Failure, Success, Try}

object AssignmentPlanCRUDController {
  val labworkAttribute = "labwork"
}

class AssignmentPlanCRUDController(val repository: SesameRepository, val sessionService: SessionHandlingService, val namespace: Namespace, val roleService: RoleService) extends AbstractCRUDController[AssignmentPlanProtocol, AssignmentPlan] {

  override protected def compareModel(input: AssignmentPlanProtocol, output: AssignmentPlan): Boolean = {
    input.attendance == output.attendance && input.mandatory == output.mandatory && input.entries == output.entries
  }

  override implicit def rdfReads: FromPG[Sesame, AssignmentPlan] = defaultBindings.AssignmentPlanBinding.assignmentPlanBinder

  override implicit def classUrisFor: ClassUrisFor[Sesame, AssignmentPlan] = defaultBindings.AssignmentPlanBinding.classUri

  override implicit def uriGenerator: UriGenerator[AssignmentPlan] = AssignmentPlan

  override implicit def rdfWrites: ToPG[Sesame, AssignmentPlan] = defaultBindings.AssignmentPlanBinding.assignmentPlanBinder

  override protected def atomize(output: AssignmentPlan): Try[Option[JsValue]] = Success(Some(Json.toJson(output)))

  override protected def fromInput(input: AssignmentPlanProtocol, existing: Option[AssignmentPlan]): AssignmentPlan = existing match {
    case Some(ap) => AssignmentPlan(input.labwork, input.attendance, input.mandatory, input.entries, ap.id)
    case None => AssignmentPlan(input.labwork, input.attendance, input.mandatory, input.entries, AssignmentPlan.randomUUID)
  }

  override implicit def reads: Reads[AssignmentPlanProtocol] = AssignmentPlan.reads

  override implicit def writes: Writes[AssignmentPlan] = AssignmentPlan.writes

  override protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[AssignmentPlan]): Try[Set[AssignmentPlan]] = {
    import AssignmentPlanCRUDController._

    queryString.foldRight(Try[Set[AssignmentPlan]](all)) {
      case ((`labworkAttribute`, v), t) => t flatMap (set => Try(UUID.fromString(v.head)).map(p => set.filter(_.labwork == p)))
      case ((_, _), set) => Failure(new Throwable("Unknown attribute"))
    }
  }

  override implicit val mimeType: LwmMimeType = LwmMimeType.assignmentPlanV1Json

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case _ => PartialSecureBlock(Permissions.god)
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case Create => SecureBlock(restrictionId, Permissions.assignmentPlan.create)
    case Update => SecureBlock(restrictionId, Permissions.assignmentPlan.update)
    case Delete => SecureBlock(restrictionId, Permissions.assignmentPlan.delete)
    case Get => SecureBlock(restrictionId, Permissions.assignmentPlan.get)
    case GetAll => SecureBlock(restrictionId, Permissions.assignmentPlan.getAll)
  }

  def createFrom(course: String) = restrictedContext(course)(Create) asyncContentTypedAction { request =>
    val newRequest = AbstractCRUDController.rebaseUri(request, AssignmentPlan.generateBase)
    super.create(NonSecureBlock)(newRequest)
  }

  def updateFrom(course: String, assignmentPlan: String) = restrictedContext(course)(Update) asyncContentTypedAction { request =>
    val newRequest = AbstractCRUDController.rebaseUri(request, AssignmentPlan.generateBase(UUID.fromString(assignmentPlan)))
    super.update(assignmentPlan, NonSecureBlock)(newRequest)
  }

  def allFrom(course: String) = restrictedContext(course)(GetAll) asyncAction { request =>
    val newRequest = AbstractCRUDController.rebaseUri(request, AssignmentPlan.generateBase)
    super.all(NonSecureBlock)(newRequest)
  }

  def getFrom(course: String, assignmentPlan: String) = restrictedContext(course)(Get) asyncAction { request =>
    val newRequest = AbstractCRUDController.rebaseUri(request, AssignmentPlan.generateBase(UUID.fromString(assignmentPlan)))
    super.get(assignmentPlan, NonSecureBlock)(newRequest)
  }

  def deleteFrom(course: String, assignmentPlan: String) = restrictedContext(course)(Delete) asyncAction { request =>
    val newRequest = AbstractCRUDController.rebaseUri(request, AssignmentPlan.generateBase(UUID.fromString(assignmentPlan)))
    super.delete(assignmentPlan, NonSecureBlock)(newRequest)
  }
}
