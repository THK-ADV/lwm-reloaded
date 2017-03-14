package models

import java.util.UUID

import controllers.JsonSerialisation
import org.joda.time.DateTime
import play.api.libs.functional.syntax._
import play.api.libs.json._
import utils.Ops.JsPathX

case class Group(label: String, labwork: UUID, members: Set[UUID], invalidated: Option[DateTime] = None, id: UUID = Group.randomUUID) extends UniqueEntity

case class GroupProtocol(label: String, labwork: UUID, members: Set[UUID])

case class GroupAtom(label: String, labwork: SesameLabwork, members: Set[SesameStudent], invalidated: Option[DateTime] = None, id: UUID) extends UniqueEntity

object Group extends UriGenerator[Group] with JsonSerialisation[GroupProtocol, Group, GroupAtom] {

  lazy val empty = Group("", UUID.randomUUID, Set.empty[UUID])

  override implicit def reads: Reads[GroupProtocol] = Json.reads[GroupProtocol]

  override implicit def writes: Writes[Group] = Json.writes[Group]

  override implicit def writesAtom: Writes[GroupAtom] = GroupAtom.writesAtom

  implicit def protocolWrites: Writes[GroupProtocol] = Json.writes[GroupProtocol]

  override def base: String = "groups"
}

object GroupAtom {

  implicit def writesAtom: Writes[GroupAtom] = (
    (JsPath \ "label").write[String] and
      (JsPath \ "labwork").write[SesameLabwork] and
      (JsPath \ "members").writeSet[SesameStudent](SesameStudent.writes) and
      (JsPath \ "invalidated").writeNullable[DateTime] and
      (JsPath \ "id").write[UUID]
    ) (unlift(GroupAtom.unapply))
}