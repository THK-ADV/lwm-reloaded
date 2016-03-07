package models

import java.util.UUID

import controllers.crud.JsonSerialisation
import models.users.Student
import play.api.libs.json.{Format, Json, Reads, Writes}

case class Group(label: String, labwork: UUID, members: Set[UUID], id: UUID = Group.randomUUID) extends UniqueEntity

case class GroupProtocol(label: String, labwork: UUID, members: Set[UUID])

case class GroupAtom(label: String, labwork: Labwork, members: Set[Student], id: UUID)

sealed abstract class GroupConstraints(val labwork: UUID)

case class GroupRangeProtocol(override val labwork: UUID, min: Int, max: Int) extends GroupConstraints(labwork)

case class GroupCountProtocol(override val labwork: UUID, count: Int) extends GroupConstraints(labwork)

object Group extends UriGenerator[Group] with JsonSerialisation[GroupProtocol, Group] {

  override implicit def reads: Reads[GroupProtocol] = Json.reads[GroupProtocol]

  override implicit def writes: Writes[Group] = Json.writes[Group]

  implicit def format: Format[Group] = Json.format[Group]

  implicit def atomicWrites: Writes[GroupAtom] = Json.writes[GroupAtom]

  override def base: String = "groups"
}

object GroupRangeProtocol {

  implicit def reads: Reads[GroupRangeProtocol] = Json.reads[GroupRangeProtocol]
}

object GroupCountProtocol {

  implicit def reads: Reads[GroupCountProtocol] = Json.reads[GroupCountProtocol]
}