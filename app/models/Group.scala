package models

import java.util.UUID

import controllers.crud.JsonSerialisation
import play.api.libs.json.{Json, Reads, Writes}

case class Group(label: String, labwork: UUID, members: Set[UUID], id: UUID = Group.randomUUID) extends UniqueEntity

case class GroupProtocol(label: String, labwork: UUID, members: Set[UUID])

sealed abstract class GroupConstraints(labwork: UUID)

case class GroupSizeProtocol(labwork: UUID, min: Int, max: Int) extends GroupConstraints(labwork)

case class GroupCountProtocol(labwork: UUID, count: Int) extends GroupConstraints(labwork)

object Group extends UriGenerator[Group] with JsonSerialisation[GroupProtocol, Group] {

  override implicit def reads: Reads[GroupProtocol] = Json.reads[GroupProtocol]

  override implicit def writes: Writes[Group] = Json.writes[Group]

  override def base: String = "groups"
}

object GroupSizeProtocol {

  implicit def reads: Reads[GroupSizeProtocol] = Json.reads[GroupSizeProtocol]
}

object GroupCountProtocol {

  implicit def reads: Reads[GroupCountProtocol] = Json.reads[GroupCountProtocol]
}