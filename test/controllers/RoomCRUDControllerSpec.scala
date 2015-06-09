package controllers

import models.Room
import play.api.libs.json.Writes

class RoomCRUDControllerSpec extends AbstractCRUDControllerSpec[Room] {
  override val entityToPass: Room = Room("label to pass")

  override def entityTypeName: String = "Room"

  override val controller: AbstractCRUDController[Room] = new RoomCRUDController(repository, namespace)

  override val entityToFail: Room = Room("label to fail")

  override implicit val jsonWrites: Writes[Room] = Room.writes

  override val mimeType: String = "application/json" //TODO: this should be a proper content type
}
