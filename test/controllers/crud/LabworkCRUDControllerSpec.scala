package controllers.crud

import java.util.UUID

import models._
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{JsValue, Json, Writes}
import play.api.mvc.{Action, Result, AnyContent, Request}
import utils.LWMActions.ContentTypedAction
import utils.LwmMimeType

class LabworkCRUDControllerSpec extends AbstractCRUDControllerSpec[LabworkProtocol, Labwork] {
  override val entityToPass: Labwork =
    Labwork(
      "label to pass",
      "description to pass",
      Semester.randomUUID,
      Course.randomUUID,
      Degree.randomUUID,
      AssignmentPlan(1, Set(AssignmentEntry(0, Set(EntryType("entry to pass"))))),
      Labwork.randomUUID)

  override def entityTypeName: String = "labwork"

  override val controller: AbstractCRUDController[LabworkProtocol, Labwork] = new LabworkCRUDController(repository, namespace, roleService) {

    override protected def invokeAction(act: Rule)(moduleId: Option[String]): Block = new Block((None, Set())) {
      override def secured(block: (Request[AnyContent]) => Result): Action[AnyContent] = Action(block)
      override def secureContentTyped(block: (Request[JsValue]) => Result): Action[JsValue] = ContentTypedAction(block)(mimeType)
    }

    override protected def fromInput(input: LabworkProtocol, id: Option[UUID]): Labwork = entityToPass
  }

  override val entityToFail: Labwork =
    Labwork(
      "label to fail",
      "description to fail",
      Semester.randomUUID,
      Course.randomUUID,
      Degree.randomUUID,
      AssignmentPlan(1, Set(AssignmentEntry(0, Set(EntryType("entry to fail"))))),
      Labwork.randomUUID)

  override implicit val jsonWrites: Writes[Labwork] = Labwork.writes

  override val mimeType: LwmMimeType = LwmMimeType.labworkV1Json

  override val inputJson: JsValue = Json.obj(
    "label" -> "label input",
    "description" -> "description input",
    "semester" -> Semester.randomUUID.toString,
    "course" -> Course.randomUUID.toString,
    "degree" -> Degree.randomUUID.toString,
    "assignmentPlan" -> Json.obj(
      "numberOfEntries" -> 1,
      "entries" -> Json.arr(Json.obj(
        "index" -> 0,
        "types" -> Json.arr(Json.obj(
          "value" -> "entry"
        )),
        "id" -> AssignmentEntry.randomUUID.toString
      )),
      "id" -> AssignmentPlan.randomUUID.toString
    )
  )

  import bindings.LabworkBinding._
  import ops._

  override def pointedGraph: PointedGraph[Sesame] = entityToPass.toPG
}
