package controllers

import java.util.UUID

import controllers.crud.{AbstractCRUDController, RoomCRUDController}
import models.{RoomProtocol, Room}
import play.api.libs.json.{Json, JsValue, Writes}

class RoomCRUDControllerSpec extends AbstractCRUDControllerSpec[RoomProtocol, Room] {
  override val entityToPass: Room = Room("label to pass", Room.randomUUID)

  override def entityTypeName: String = "Room"

  override val controller: AbstractCRUDController[RoomProtocol, Room] = new RoomCRUDController(repository, namespace) {
    override protected def fromInput(input: RoomProtocol, id: Option[UUID]): Room = entityToPass
  }

  override val entityToFail: Room = Room("label to fail", Room.randomUUID)

  override implicit val jsonWrites: Writes[Room] = Room.writes

  override val mimeType: String = "application/json" //TODO: this should be a proper content type

  override val inputJson: JsValue = Json.obj(
    "label" -> "label input"
  )
}
