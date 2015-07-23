package controllers.crud

import java.util.UUID

import models.{Group, GroupProtocol}
import play.api.libs.json.{JsValue, Json, Writes}
import utils.LWMMimeType

class GroupCRUDControllerSpec extends AbstractCRUDControllerSpec[GroupProtocol, Group] {
  override val entityToPass: Group = Group("groupSchedule to pass", "label to pass", "labwork to pass", Group.randomUUID)

  override def entityTypeName: String = "group"

  override val controller: AbstractCRUDController[GroupProtocol, Group] = new GroupCRUDController(repository, namespace) {
    override protected def fromInput(input: GroupProtocol, id: Option[UUID]): Group = entityToPass
  }

  override val entityToFail: Group = Group("groupSchedule to fail", "label to fail", "labwork to fail", Group.randomUUID)

  override implicit val jsonWrites: Writes[Group] = Group.writes

  override val mimeType: LWMMimeType = LWMMimeType.groupV1Json

  override val inputJson: JsValue = Json.obj(
    "groupSchedule" -> "groupSchedule input",
    "label" -> "label input",
    "labwork" -> "labwork input"
  )
}
