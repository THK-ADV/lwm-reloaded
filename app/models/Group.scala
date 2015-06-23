package models

import java.util.UUID

import controllers.crud.JsonSerialisation
import play.api.libs.json.{Json, Reads, Writes}

case class Group(groupSchedule: String, label: String, labwork: String, id: UUID) extends UniqueEntity

case class GroupProtocol(groupSchedule: String, label: String, labwork: String)

// not included members: []

object Group extends UriGenerator[Group] with JsonSerialisation[GroupProtocol, Group] {

  override implicit def reads: Reads[GroupProtocol] = Json.reads[GroupProtocol]

  override implicit def writes: Writes[Group] = Json.writes[Group]

  override def base: String = "groups"
}
