package controllers.crud

import java.util.UUID

import models._
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Json, Writes, JsValue}
import utils.LwmMimeType

class AssignmentPlanCRUDControllerSpec extends AbstractCRUDControllerSpec[AssignmentPlanProtocol, AssignmentPlan] {

  override def entityTypeName: String = "assignmentPlan"

  override val controller: AbstractCRUDController[AssignmentPlanProtocol, AssignmentPlan] = new AssignmentPlanCRUDController(repository, sessionService, namespace, roleService) {

    override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }

    override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }

    override protected def fromInput(input: AssignmentPlanProtocol, existing: Option[AssignmentPlan]): AssignmentPlan = entityToPass
  }

  def entries(s: String): Set[AssignmentEntry] = (0 until 5).map( n =>
    AssignmentEntry(n, s"${n.toString} to $s", AssignmentEntryType.all)
  ).toSet

  val labworkToPass = Labwork("label to pass", "desc to pass", UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID())
  val labworkToFail = Labwork("label to fail", "desc to fail", UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID())

  override val entityToFail: AssignmentPlan = AssignmentPlan(labworkToFail.id, 5, 5, entries("fail"))

  override val entityToPass: AssignmentPlan = AssignmentPlan(labworkToPass.id, 5, 5, entries("pass"))

  override implicit val jsonWrites: Writes[AssignmentPlan] = AssignmentPlan.writes

  override val mimeType: LwmMimeType = LwmMimeType.assignmentPlanV1Json

  import ops._
  import bindings.AssignmentPlanBinding.assignmentPlanBinder
  override val pointedGraph: PointedGraph[Sesame] = entityToPass.toPG

  override val updateJson: JsValue = Json.obj(
    "labwork" -> entityToPass.labwork,
    "attendance" -> entityToPass.attendance,
    "mandatory" -> entityToPass.mandatory,
    "entries" -> entityToPass.entries.drop(2)
  )

  override val inputJson: JsValue = Json.obj(
    "labwork" -> entityToPass.labwork,
    "attendance" -> entityToPass.attendance,
    "mandatory" -> entityToPass.mandatory,
    "entries" -> entityToPass.entries
  )
}
