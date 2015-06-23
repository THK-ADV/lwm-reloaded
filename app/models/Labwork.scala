package models

import java.util.UUID
import controllers.crud.JsonSerialisation
import play.api.libs.json.{Json, Reads, Writes}
import store.Namespace

case class Labwork(label: String, id: UUID = UUID.randomUUID()) extends UniqueEntity

object Labwork extends UriGenerator[Labwork] with JsonSerialisation[Labwork] {
  def generateUri(labwork: Labwork)(implicit ns: Namespace): String = s"${ns}labworks/${labwork.id}"

  override implicit def reads: Reads[Labwork] = Json.reads[Labwork]

  override implicit def writes: Writes[Labwork] = Json.writes[Labwork]
}