package models

import java.util.UUID
import controllers.crud.JsonSerialisation
import play.api.libs.json.{Json, Reads, Writes}
import store.Namespace

case class Group(groupSchedule: String, label: String, labwork: String, id: Option[UUID] = Some(UUID.randomUUID())) extends UniqueEntity

// not included members: []

object Group extends UriGenerator[Group] with JsonSerialisation[Group] {

  override implicit def reads: Reads[Group] = Json.reads[Group]

  override implicit def writes: Writes[Group] = Json.writes[Group]

  override def base: String = "groups"
}
