package controllers

import java.util.UUID

import controllers.crud.{GroupCRUDController, AbstractCRUDController}
import models.{GroupProtocol, Group}
import play.api.libs.json.{Json, JsValue, Writes}

class GroupCRUDControllerSpec extends AbstractCRUDControllerSpec[GroupProtocol, Group] {
  override val entityToPass: Group = Group("groupSchedule to pass", "label to pass", "labwork to pass", Group.randomUUID)

  override def entityTypeName: String = "Group"

  override val controller: AbstractCRUDController[GroupProtocol, Group] = new GroupCRUDController(repository, namespace) {
    override protected def fromInput(input: GroupProtocol, id: Option[UUID]): Group = entityToPass
  }

  override val entityToFail: Group = Group("groupSchedule to fail", "label to fail", "labwork to fail", Group.randomUUID)

  override implicit val jsonWrites: Writes[Group] = Group.writes

  override val mimeType: String = "application/json" //TODO: this should be a proper content type

  override val inputJson: JsValue = Json.obj(
    "groupSchedule" -> "groupSchedule input",
    "label" -> "label input",
    "labwork" -> "labwork input"
  )
}
