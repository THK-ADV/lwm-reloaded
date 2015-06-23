package models.timetable

import java.util.UUID
import controllers.crud.JsonSerialisation
import models.{UniqueEntity, UriGenerator}
import play.api.libs.json.{Json, Reads, Writes}
import store.Namespace

case class Timetable(id: Option[UUID] = Some(UUID.randomUUID())) extends UniqueEntity

//not included entries:[]

object Timetable extends UriGenerator[Timetable] with JsonSerialisation[Timetable] {

  override implicit def reads: Reads[Timetable] = Json.reads[Timetable]

  override implicit def writes: Writes[Timetable] = Json.writes[Timetable]

  override def base: String = "timetables"

}