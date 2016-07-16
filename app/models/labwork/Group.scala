package models.labwork

import java.util.UUID

import controllers.crud.JsonSerialisation
import models.users.Student
import models.{UniqueEntity, UriGenerator}
import org.joda.time.DateTime
import play.api.libs.json.{Format, Json, Reads, Writes}

case class Group(label: String, labwork: UUID, members: Set[UUID], invalidated: Option[DateTime] = None, id: UUID = Group.randomUUID) extends UniqueEntity

case class GroupProtocol(label: String, labwork: UUID, members: Set[UUID])

case class GroupAtom(label: String, labwork: Labwork, members: Set[Student], invalidated: Option[DateTime] = None, id: UUID) extends UniqueEntity

object Group extends UriGenerator[Group] with JsonSerialisation[GroupProtocol, Group, GroupAtom] {

  lazy val empty = Group("", UUID.randomUUID, Set.empty[UUID])

  override implicit def reads: Reads[GroupProtocol] = Json.reads[GroupProtocol]

  override implicit def writes: Writes[Group] = Json.writes[Group]

  override implicit def writesAtom: Writes[GroupAtom] = Json.writes[GroupAtom]

  implicit def protocolWrites: Writes[GroupProtocol] = Json.writes[GroupProtocol]

  implicit def format: Format[Group] = Json.format[Group]

  override def base: String = "groups"
}