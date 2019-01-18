package models

import java.sql.Timestamp
import java.util.UUID

import org.joda.time.DateTime
import play.api.libs.functional.syntax._
import play.api.libs.json._
import utils.LwmDateTime
import utils.LwmDateTime._
import utils.Ops.JsPathX

sealed trait LabworkApplication extends UniqueEntity {
  def lastModified: DateTime
}

case class PostgresLabworkApplication(labwork: UUID, applicant: UUID, friends: Set[UUID], lastModified: DateTime, id: UUID = UUID.randomUUID) extends LabworkApplication

case class PostgresLabworkApplicationProtocol(labwork: UUID, applicant: UUID, friends: Set[UUID])

case class PostgresLabworkApplicationAtom(labwork: PostgresLabworkAtom, applicant: User, friends: Set[User], lastModified: DateTime, id: UUID) extends LabworkApplication

case class LabworkApplicationDb(labwork: UUID, applicant: UUID, friends: Set[UUID], lastModified: Timestamp = DateTime.now.timestamp, invalidated: Option[Timestamp] = None, id: UUID = UUID.randomUUID) extends UniqueDbEntity {
  override def toLwmModel = PostgresLabworkApplication(labwork, applicant, friends, lastModified.dateTime, id)
}

case class LabworkApplicationFriend(labworkApplication: UUID, friend: UUID, lastModified: Timestamp = DateTime.now.timestamp, invalidated: Option[Timestamp] = None, id: UUID = UUID.randomUUID) extends UniqueDbEntity {
  override def toLwmModel = this
}

object PostgresLabworkApplication {
  implicit val writes: Writes[PostgresLabworkApplication] = Json.writes[PostgresLabworkApplication]
}

object PostgresLabworkApplicationProtocol {
  implicit val reads: Reads[PostgresLabworkApplicationProtocol] = Json.reads[PostgresLabworkApplicationProtocol]
}

object LabworkApplicationDb {
  def from(protocol: PostgresLabworkApplicationProtocol, existingId: Option[UUID]) = {
    LabworkApplicationDb(protocol.labwork, protocol.applicant, protocol.friends, id = existingId getOrElse UUID.randomUUID)
  }
}

object LabworkApplication {

  implicit val writes: Writes[LabworkApplication] = new Writes[LabworkApplication] {
    override def writes(labworkApplication: LabworkApplication) = labworkApplication match {
      case normal: PostgresLabworkApplication => Json.toJson(normal)(PostgresLabworkApplication.writes)
      case atom: PostgresLabworkApplicationAtom => Json.toJson(atom)(PostgresLabworkApplicationAtom.writes)
    }
  }
}

object PostgresLabworkApplicationAtom {

  implicit val writes: Writes[PostgresLabworkApplicationAtom] = (
    (JsPath \ "labwork").write[PostgresLabworkAtom](PostgresLabworkAtom.writes) and
      (JsPath \ "applicant").write[User] and
      (JsPath \ "friends").writeSet[User] and
      (JsPath \ "lastModified").write[DateTime](LwmDateTime.writeDateTime) and
      (JsPath \ "id").write[UUID]
    ) (unlift(PostgresLabworkApplicationAtom.unapply))
}