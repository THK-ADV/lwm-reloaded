package controllers.crud

import java.util.UUID

import models.{Room, RoomProtocol}
import play.api.libs.json.{JsValue, Json, Writes}
import utils.LWMMimeType

class RoomCRUDControllerSpec extends AbstractCRUDControllerSpec[RoomProtocol, Room] {
  override val entityToPass: Room = Room("label to pass", Room.randomUUID)

  override def entityTypeName: String = "room"

  override val controller: AbstractCRUDController[RoomProtocol, Room] = new RoomCRUDController(repository, namespace) {
    override protected def fromInput(input: RoomProtocol, id: Option[UUID]): Room = entityToPass
  }

  override val entityToFail: Room = Room("label to fail", Room.randomUUID)

  override implicit val jsonWrites: Writes[Room] = Room.writes

  override val mimeType: LWMMimeType = LWMMimeType.roomV1Json

  override val inputJson: JsValue = Json.obj(
    "label" -> "label input"
  )
}
