package controllers

import models.Group
import play.api.libs.json.Writes

class GroupCRUDControllerSpec extends AbstractCRUDControllerSpec[Group] {
  override val entityToPass: Group = Group("groupSchedule to pass", "label to pass", "labwork to pass")

  override def entityTypeName: String = "Group"

  override val controller: AbstractCRUDController[Group] = new GroupCRUDController(repository, namespace)

  override val entityToFail: Group = Group("groupSchedule to fail", "label to fail", "labwork to fail")

  override implicit val jsonWrites: Writes[Group] = Group.writes

  override val mimeType: String = "application/json" //TODO: this should be a proper content type
}
