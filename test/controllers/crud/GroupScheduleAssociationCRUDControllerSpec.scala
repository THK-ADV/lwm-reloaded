package controllers.crud

import java.util.UUID

import models.schedules.{GroupScheduleAssociation, GroupScheduleAssociationProtocol}
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{JsValue, Json, Writes}
import utils.LwmMimeType

class GroupScheduleAssociationCRUDControllerSpec extends AbstractCRUDControllerSpec[GroupScheduleAssociationProtocol, GroupScheduleAssociation] {
  override val entityToPass: GroupScheduleAssociation = GroupScheduleAssociation("date to pass", "timetableEntry to pass", GroupScheduleAssociation.randomUUID)

  override def entityTypeName: String = "groupScheduleAssociation"

  override val controller: AbstractCRUDController[GroupScheduleAssociationProtocol, GroupScheduleAssociation] = new GroupScheduleAssociationCRUDController(repository, namespace, roleService) {
    override protected def fromInput(input: GroupScheduleAssociationProtocol, id: Option[UUID]): GroupScheduleAssociation = entityToPass
  }

  override val entityToFail: GroupScheduleAssociation = GroupScheduleAssociation("date to fail", "timetableEntry to fail", GroupScheduleAssociation.randomUUID)

  override implicit val jsonWrites: Writes[GroupScheduleAssociation] = GroupScheduleAssociation.writes

  override val mimeType: LwmMimeType = LwmMimeType.groupScheduleAssociationV1Json

  override val inputJson: JsValue = Json.obj(
    "date" -> "date input",
    "timetableEntry" -> "timetableEntry input"
  )

  import ops._
  import bindings.GroupScheduleAssociationBinding._

  override def pointedGraph: PointedGraph[Sesame] = entityToPass.toPG
}
