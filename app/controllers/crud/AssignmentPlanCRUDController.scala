package controllers.crud

import java.util.UUID

import models._
import models.security.Permissions
import org.w3.banana.binder.{FromPG, ClassUrisFor, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Json, JsValue, Reads, Writes}
import services.RoleService
import store.{Namespace, SesameRepository}
import utils.LwmMimeType
import scala.collection.Map
import scala.util.{Failure, Success, Try}

object AssignmentPlanCRUDController {
  val labworkAttribute = "labwork"
}

class AssignmentPlanCRUDController(val repository: SesameRepository, val namespace: Namespace, val roleService: RoleService) extends AbstractCRUDController[AssignmentPlanProtocol, AssignmentPlan] {

  override protected def compareModel(input: AssignmentPlanProtocol, output: AssignmentPlan): Boolean = {
    import models.AssignmentEntry.toProtocol
    input.attendance == output.attendance && input.mandatory == output.mandatory && input.entries == output.entries.map(toProtocol)
  }

  override implicit def rdfReads: FromPG[Sesame, AssignmentPlan] = defaultBindings.AssignmentPlanBinding.assignmentPlanBinder

  override implicit def classUrisFor: ClassUrisFor[Sesame, AssignmentPlan] = defaultBindings.AssignmentPlanBinding.classUri

  override implicit def uriGenerator: UriGenerator[AssignmentPlan] = AssignmentPlan

  override implicit def rdfWrites: ToPG[Sesame, AssignmentPlan] = defaultBindings.AssignmentPlanBinding.assignmentPlanBinder

  override protected def atomize(output: AssignmentPlan): Try[Option[JsValue]] = Success(Some(Json.toJson(output)))

  // TODO REFACTOR, CONSIDER FILTER(SAME_X).FLATMAP
  override protected def fromInput(input: AssignmentPlanProtocol, existing: Option[AssignmentPlan]): AssignmentPlan = {
    def zip(left: AssignmentPlanProtocol, right: AssignmentPlan) = {
      val i = left.entries.toVector.sortBy(_.index)
      val e = right.entries.toVector.sortBy(_.index)
      i.zip(e)
    }

    def fromEntryInput(input: AssignmentEntryProtocol, existing: Option[AssignmentEntry]): AssignmentEntry = {
      def sameType(l: AssignmentEntryType, r: AssignmentEntryTypeProtocol) = {
        l.entryType == r.entryType && l.bool == r.bool && l.int == r.int
      }

      existing match {
        case Some(e) =>
          val types = input.types.map(t => AssignmentEntryType(t.entryType, t.bool, t.int, e.types.filter(tt => sameType(tt, t)).head.id))
          AssignmentEntry(input.index, input.label, types, input.duration, e.id)
        case None =>
          AssignmentEntry(input.index, input.label, input.types.map(t => fromTypeInput(t, None)), input.duration, AssignmentEntry.randomUUID)
      }
    }

    def fromTypeInput(input: AssignmentEntryTypeProtocol, existing: Option[AssignmentEntryType]): AssignmentEntryType = existing match {
      case Some(t) => AssignmentEntryType(input.entryType, input.bool, input.int, t.id)
      case None => AssignmentEntryType.fromProtocol(input)
    }

    existing match {
      case Some(ap) =>
        AssignmentPlan(input.labwork, input.attendance, input.mandatory, zip(input, ap).map(e => fromEntryInput(e._1, Some(e._2))).toSet, ap.id)
      case None =>
        AssignmentPlan(input.labwork, input.attendance, input.mandatory, input.entries.map(e => fromEntryInput(e, None)), AssignmentPlan.randomUUID)
    }
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
