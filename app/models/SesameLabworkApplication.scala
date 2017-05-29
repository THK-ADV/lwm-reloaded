package models

import java.sql.Timestamp
import java.util.UUID

import controllers.JsonSerialisation
import org.joda.time.DateTime
import play.api.libs.functional.syntax._
import play.api.libs.json._
import utils.Ops.JsPathX
import models.LwmDateTime._

case class SesameLabworkApplication(labwork: UUID, applicant: UUID, friends: Set[UUID], timestamp: DateTime = DateTime.now, invalidated: Option[DateTime] = None, id: UUID = SesameLabworkApplication.randomUUID) extends UniqueEntity {

  override def equals(obj: scala.Any): Boolean = obj match {
    case SesameLabworkApplication(l, a, f, t, _, i) => l == labwork && a == applicant && f == friends && t.isEqual(timestamp) && i == id
    case _ => false
  }
}

case class SesameLabworkApplicationProtocol(labwork: UUID, applicant: UUID, friends: Set[UUID])

case class SesameLabworkApplicationAtom(labwork: SesameLabworkAtom, applicant: SesameStudent, friends: Set[SesameStudent], timestamp: DateTime, invalidated: Option[DateTime] = None, id: UUID) extends UniqueEntity

object SesameLabworkApplication extends UriGenerator[SesameLabworkApplication] with JsonSerialisation[SesameLabworkApplicationProtocol, SesameLabworkApplication, SesameLabworkApplicationAtom] {

  override def base: String = "labworkApplications"

  override implicit def reads: Reads[SesameLabworkApplicationProtocol] = Json.reads[SesameLabworkApplicationProtocol]

  override implicit def writes: Writes[SesameLabworkApplication] = (
    (JsPath \ "labwork").write[UUID] and
      (JsPath \ "applicant").write[UUID] and
      (JsPath \ "friends").writeSet[UUID] and
      (JsPath \ "timestamp").write[DateTime](LwmDateTime.writes) and
      (JsPath \ "invalidated").writeNullable[DateTime] and
      (JsPath \ "id").write[UUID]
    ) (unlift(SesameLabworkApplication.unapply))

  override implicit def writesAtom: Writes[SesameLabworkApplicationAtom] = SesameLabworkApplicationAtom.writesAtom
}

object SesameLabworkApplicationAtom {

  implicit def writesAtom: Writes[SesameLabworkApplicationAtom] = (
    (JsPath \ "labwork").write[SesameLabworkAtom] and
      (JsPath \ "applicant").write[SesameStudent](SesameStudent.writes) and
      (JsPath \ "friends").writeSet[SesameStudent](SesameStudent.writes) and
      (JsPath \ "timestamp").write[DateTime](LwmDateTime.writes) and
      (JsPath \ "invalidated").writeNullable[DateTime] and
      (JsPath \ "id").write[UUID]
    ) (unlift(SesameLabworkApplicationAtom.unapply))
}

// Postgres

sealed trait LabworkApplication extends UniqueEntity

case class PostgresLabworkApplication(labwork: UUID, applicant: UUID, friends: Set[UUID], timestamp: DateTime = DateTime.now, id: UUID = UUID.randomUUID) extends LabworkApplication

case class PostgresLabworkApplicationProtocol(labwork: UUID, applicant: UUID, friends: Set[UUID])

case class PostgresLabworkApplicationAtom(labwork: PostgresLabworkAtom, applicant: User, friends: Set[User], timestamp: DateTime, id: UUID) extends LabworkApplication

case class LabworkApplicationDb(labwork: UUID, applicant: UUID, friends: Set[UUID], timestamp: Timestamp = DateTime.now.timestamp, lastModified: Timestamp = DateTime.now.timestamp, invalidated: Option[Timestamp] = None, id: UUID = UUID.randomUUID) extends UniqueEntity {
  def toLabworkApplication = PostgresLabworkApplication(labwork, applicant, friends, timestamp.dateTime, id)
}

case class LabworkApplicationFriend(labworkApplication: UUID, friend: UUID, lastModified: Timestamp = DateTime.now.timestamp, invalidated: Option[Timestamp] = None, id: UUID = UUID.randomUUID) extends UniqueEntity

object PostgresLabworkApplication extends JsonSerialisation[PostgresLabworkApplicationProtocol, PostgresLabworkApplication, PostgresLabworkApplicationAtom] {
  override implicit def reads: Reads[PostgresLabworkApplicationProtocol] = Json.reads[PostgresLabworkApplicationProtocol]

  override implicit def writes: Writes[PostgresLabworkApplication] = Json.writes[PostgresLabworkApplication]

  override implicit def writesAtom: Writes[PostgresLabworkApplicationAtom] = PostgresLabworkApplicationAtom.writesAtom
}

object LabworkApplicationDb {
  def from(protocol: PostgresLabworkApplicationProtocol, existingId: Option[UUID]) = {
    LabworkApplicationDb(protocol.labwork, protocol.applicant, protocol.friends)
  }
}

object LabworkApplication {

  implicit def writes: Writes[LabworkApplication] = new Writes[LabworkApplication] {
    override def writes(labworkApplication: LabworkApplication) = labworkApplication match {
      case postgresLabworkApplication: PostgresLabworkApplication => Json.toJson(postgresLabworkApplication)(PostgresLabworkApplication.writes)
      case postgresLabworkApplicationAtom: PostgresLabworkApplicationAtom => Json.toJson(postgresLabworkApplicationAtom)(PostgresLabworkApplication.writesAtom)
    }
  }
}

object PostgresLabworkApplicationAtom {

  implicit def writesAtom: Writes[PostgresLabworkApplicationAtom] = (
    (JsPath \ "labwork").write[PostgresLabworkAtom](PostgresLabworkAtom.writesAtom) and
      (JsPath \ "applicant").write[User] and
      (JsPath \ "friends").writeSet[User] and
      (JsPath \ "timestamp").write[DateTime](LwmDateTime.writes) and
      (JsPath \ "id").write[UUID]
    ) (unlift(PostgresLabworkApplicationAtom.unapply))
}