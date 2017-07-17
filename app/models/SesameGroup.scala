package models

import java.sql.Timestamp
import java.util.UUID

import controllers.JsonSerialisation
import org.joda.time.DateTime
import play.api.libs.functional.syntax._
import play.api.libs.json._
import utils.Ops.JsPathX
import models.LwmDateTime._

case class SesameGroup(label: String, labwork: UUID, members: Set[UUID], invalidated: Option[DateTime] = None, id: UUID = SesameGroup.randomUUID) extends UniqueEntity

case class SesameGroupProtocol(label: String, labwork: UUID, members: Set[UUID])

case class SesameGroupAtom(label: String, labwork: SesameLabwork, members: Set[SesameStudent], invalidated: Option[DateTime] = None, id: UUID) extends UniqueEntity

object SesameGroup extends UriGenerator[SesameGroup] with JsonSerialisation[SesameGroupProtocol, SesameGroup, SesameGroupAtom] {

  lazy val empty = SesameGroup("", UUID.randomUUID, Set.empty[UUID])

  override implicit def reads: Reads[SesameGroupProtocol] = Json.reads[SesameGroupProtocol]

  override implicit def writes: Writes[SesameGroup] = Json.writes[SesameGroup]

  override implicit def writesAtom: Writes[SesameGroupAtom] = SesameGroupAtom.writesAtom

  implicit def protocolWrites: Writes[SesameGroupProtocol] = Json.writes[SesameGroupProtocol]

  override def base: String = "groups"
}

object SesameGroupAtom {

  implicit def writesAtom: Writes[SesameGroupAtom] = (
    (JsPath \ "label").write[String] and
      (JsPath \ "labwork").write[SesameLabwork] and
      (JsPath \ "members").writeSet[SesameStudent](SesameStudent.writes) and
      (JsPath \ "invalidated").writeNullable[DateTime] and
      (JsPath \ "id").write[UUID]
    ) (unlift(SesameGroupAtom.unapply))
}

// POSTGRES

sealed trait Group extends UniqueEntity

case class PostgresGroup(label: String, labwork: UUID, members: Set[UUID], id: UUID = UUID.randomUUID) extends Group

case class PostgresGroupAtom(label: String, labwork: PostgresLabwork, members: Set[User], id: UUID) extends Group

case class PostgresGroupProtocol(label: String, labwork: UUID, members: Set[UUID])

object PostgresGroup extends JsonSerialisation[PostgresGroupProtocol, PostgresGroup, PostgresGroupAtom] {
  override implicit def reads = Json.reads[PostgresGroupProtocol]

  override implicit def writes = Json.writes[PostgresGroup]

  override implicit def writesAtom = Json.writes[PostgresGroupAtom]
}

// DB

case class GroupDb(label: String, labwork: UUID, members: Set[UUID], lastModified: Timestamp = DateTime.now.timestamp, invalidated: Option[Timestamp] = None, id: UUID = UUID.randomUUID) extends UniqueDbEntity {
  override def toLwmModel = PostgresGroup(label, labwork, members, id)
}

case class GroupMembership(group: UUID, student: UUID, id: UUID = UUID.randomUUID) extends UniqueEntity