package controllers

import java.util.UUID

import controllers.crud.{TimetableEntryCRUDController, AbstractCRUDController}
import models.timetable.{TimetableEntryProtocol, TimetableEntry}
import play.api.libs.json.{Json, JsValue, Writes}

class TimetableEntryCRUDControllerSpec extends AbstractCRUDControllerSpec[TimetableEntryProtocol, TimetableEntry] {
  override val entityToPass: TimetableEntry = TimetableEntry("supervisor to pass", "room to pass", "startTime to pass", "endTime to pass", TimetableEntry.randomUUID)

  override def entityTypeName: String = "TimetableEntry"

  override val controller: AbstractCRUDController[TimetableEntryProtocol, TimetableEntry] = new TimetableEntryCRUDController(repository, namespace) {
    override protected def fromInput(input: TimetableEntryProtocol, id: Option[UUID]): TimetableEntry = entityToPass
  }

  override val entityToFail: TimetableEntry = TimetableEntry("supervisor to fail", "room to fail", "startTime to fail", "endTime to fail", TimetableEntry.randomUUID)

  override implicit val jsonWrites: Writes[TimetableEntry] = TimetableEntry.writes

  override val mimeType: String = "application/json" //TODO: this should be a proper content type

  override val inputJson: JsValue = Json.obj(
    "supervisor" -> "supervisor input",
    "room" -> "room input",
    "startTime" -> "startTime input",
    "endTime" -> "endTime input"
  )
}
