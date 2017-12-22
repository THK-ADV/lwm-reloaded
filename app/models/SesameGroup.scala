package models

import java.sql.Timestamp
import java.util.UUID

import org.joda.time.DateTime
import play.api.libs.functional.syntax._
import play.api.libs.json._
import utils.LwmDateTime._
import utils.Ops.JsPathX

case class SesameGroup(label: String, labwork: UUID, members: Set[UUID], invalidated: Option[DateTime] = None, id: UUID = SesameGroup.randomUUID) extends UniqueEntity

case class SesameGroupProtocol(label: String, labwork: UUID, members: Set[UUID])

case class SesameGroupAtom(label: String, labwork: SesameLabwork, members: Set[SesameStudent], invalidated: Option[DateTime] = None, id: UUID) extends UniqueEntity

object SesameGroup extends UriGenerator[SesameGroup] {
  override def base: String = "groups"
}

// POSTGRES

sealed trait Group extends UniqueEntity

case class PostgresGroup(label: String, labwork: UUID, members: Set[UUID], id: UUID = UUID.randomUUID) extends Group

case class PostgresGroupAtom(label: String, labwork: PostgresLabwork, members: Set[User], id: UUID) extends Group

case class PostgresGroupProtocol(label: String, labwork: UUID, members: Set[UUID])

object Group {

  implicit val writes: Writes[Group] = new Writes[Group] {
    override def writes(g: Group) = g match {
      case normal: PostgresGroup => Json.toJson(normal)(PostgresGroup.writes)
      case atom: PostgresGroupAtom => Json.toJson(atom)(PostgresGroupAtom.writes)
    }
  }
}

object PostgresGroup {
  implicit val writes: Writes[PostgresGroup] = Json.writes[PostgresGroup]

  implicit val reads: Reads[PostgresGroup] = Json.reads[PostgresGroup]
}

object PostgresGroupProtocol {
  implicit val reads: Reads[PostgresGroupProtocol] = Json.reads[PostgresGroupProtocol]
}

object PostgresGroupAtom {

  implicit val writes: Writes[PostgresGroupAtom] = (
    (JsPath \ "label").write[String] and
      (JsPath \ "labwork").write[PostgresLabwork](PostgresLabwork.writes) and
      (JsPath \ "members").writeSet[User](User.writes) and
      (JsPath \ "id").write[UUID]
    ) (unlift(PostgresGroupAtom.unapply))
}

// DB

case class GroupDb(label: String, labwork: UUID, members: Set[UUID], lastModified: Timestamp = DateTime.now.timestamp, invalidated: Option[Timestamp] = None, id: UUID = UUID.randomUUID) extends UniqueDbEntity {
  override def toLwmModel = PostgresGroup(label, labwork, members, id)
}

case class GroupMembership(group: UUID, student: UUID, id: UUID = UUID.randomUUID) extends UniqueEntity