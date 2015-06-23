package models

import java.util.UUID
import controllers.crud.JsonSerialisation
import play.api.libs.json.{Json, Reads, Writes}
import store.Namespace

case class Course(label: String, lecturer: String, id: Option[UUID] = Some(UUID.randomUUID())) extends UniqueEntity

object Course extends UriGenerator[Course] with JsonSerialisation[Course] {

  override implicit def reads: Reads[Course] = Json.reads[Course]

  override implicit def writes: Writes[Course] = Json.writes[Course]

  override def base: String = "courses"
}