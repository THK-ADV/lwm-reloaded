package models

import java.util.UUID
import controllers.crud.JsonSerialisation
import play.api.libs.json.{Json, Reads, Writes}
import store.Namespace

case class Degree(label: String, id: UUID = UUID.randomUUID()) extends UniqueEntity

object Degree extends UriGenerator[Degree] with JsonSerialisation[Degree]{

  def generateUri(degree: Degree)(implicit ns: Namespace): String = s"${ns}degrees/${degree.id}"

  override implicit def reads: Reads[Degree] = Json.reads[Degree]

  override implicit def writes: Writes[Degree] = Json.writes[Degree]
}