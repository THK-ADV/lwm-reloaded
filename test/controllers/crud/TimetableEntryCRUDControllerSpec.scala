package controllers.crud

import java.util.UUID

import models.timetable.{TimetableEntry, TimetableEntryProtocol}
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{JsValue, Json, Writes}
import utils.LWMMimeType

class TimetableEntryCRUDControllerSpec extends AbstractCRUDControllerSpec[TimetableEntryProtocol, TimetableEntry] {
  override val entityToPass: TimetableEntry = TimetableEntry("supervisor to pass", "room to pass", "startTime to pass", "endTime to pass", TimetableEntry.randomUUID)

  override def entityTypeName: String = "timetableEntry"

  override val controller: AbstractCRUDController[TimetableEntryProtocol, TimetableEntry] = new TimetableEntryCRUDController(repository, namespace) {
    override protected def fromInput(input: TimetableEntryProtocol, id: Option[UUID]): TimetableEntry = entityToPass
  }

  override val entityToFail: TimetableEntry = TimetableEntry("supervisor to fail", "room to fail", "startTime to fail", "endTime to fail", TimetableEntry.randomUUID)

  override implicit val jsonWrites: Writes[TimetableEntry] = TimetableEntry.writes

  override val mimeType: LWMMimeType = LWMMimeType.timetableEntryV1Json

  override val inputJson: JsValue = Json.obj(
    "supervisor" -> "supervisor input",
    "room" -> "room input",
    "startTime" -> "startTime input",
    "endTime" -> "endTime input"
  )

  import bindings.TimetableEntryBinding._
  import ops._

  override def pointedGraph: PointedGraph[Sesame] = entityToPass.toPG
}
