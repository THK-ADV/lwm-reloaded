package controllers.crud

import java.util.UUID

import models.{Room, RoomProtocol}
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{JsValue, Json, Writes}
import play.api.mvc.{Action, Result, AnyContent, Request}
import utils.LWMActions.ContentTypedAction
import utils.LwmMimeType

class RoomCRUDControllerSpec extends AbstractCRUDControllerSpec[RoomProtocol, Room] {
  override val entityToPass: Room = Room("label to pass", Room.randomUUID)

  override def entityTypeName: String = "room"

  override val controller: AbstractCRUDController[RoomProtocol, Room] = new RoomCRUDController(repository, namespace, roleService) {

    override protected def invokeAction(act: Rule)(moduleId: Option[String]): Block = new Block((None, Set())) {
      override def secured(block: (Request[AnyContent]) => Result): Action[AnyContent] = Action(block)
      override def secureContentTyped(block: (Request[JsValue]) => Result): Action[JsValue] = ContentTypedAction(block)(mimeType)
    }

    override protected def fromInput(input: RoomProtocol, id: Option[UUID]): Room = entityToPass
  }

  override val entityToFail: Room = Room("label to fail", Room.randomUUID)

  override implicit val jsonWrites: Writes[Room] = Room.writes

  override val mimeType: LwmMimeType = LwmMimeType.roomV1Json

  override val inputJson: JsValue = Json.obj(
    "label" -> "label input"
  )

  import bindings.RoomBinding._
  import ops._

  override def pointedGraph: PointedGraph[Sesame] = entityToPass.toPG
}
