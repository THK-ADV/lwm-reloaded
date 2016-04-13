package models.labwork

import java.util.UUID

import controllers.crud.JsonSerialisation
import models.users.Student
import models.{UniqueEntity, UriGenerator}
import play.api.libs.json.{Format, Json, Reads, Writes}

case class Group(label: String, labwork: UUID, members: Set[UUID], id: UUID = Group.randomUUID) extends UniqueEntity

case class GroupProtocol(label: String, labwork: UUID, members: Set[UUID])

case class GroupAtom(label: String, labwork: Labwork, members: Set[Student], id: UUID)

object Group extends UriGenerator[Group] with JsonSerialisation[GroupProtocol, Group] {

  lazy val empty = Group("", UUID.randomUUID, Set.empty[UUID])

  override implicit def reads: Reads[GroupProtocol] = Json.reads[GroupProtocol]

  override implicit def writes: Writes[Group] = Json.writes[Group]

  implicit def protocolWrites: Writes[GroupProtocol] = Json.writes[GroupProtocol]

  implicit def format: Format[Group] = Json.format[Group]

  implicit def atomicWrites: Writes[GroupAtom] = Json.writes[GroupAtom]

  override def base: String = "groups"
}