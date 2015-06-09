package controllers

import models.timetable.TimetableEntry
import play.api.libs.json.Writes

class TimetableEntryCRUDControllerSpec extends AbstractCRUDControllerSpec[TimetableEntry] {
  override val entityToPass: TimetableEntry = TimetableEntry("supervisor to pass", "room to pass", "startTime to pass", "endTime to pass")

  override def entityTypeName: String = "TimetableEntry"

  override val controller: AbstractCRUDController[TimetableEntry] = new TimetableEntryCRUDController(repository, namespace)

  override val entityToFail: TimetableEntry = TimetableEntry("supervisor to fail", "room to fail", "startTime to fail", "endTime to fail")

  override implicit val jsonWrites: Writes[TimetableEntry] = TimetableEntry.writes

  override val mimeType: String = "application/json" //TODO: this should be a proper content type
}
