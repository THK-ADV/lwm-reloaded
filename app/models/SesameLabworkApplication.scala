package models

import java.util.UUID

import controllers.JsonSerialisation
import org.joda.time.DateTime
import play.api.libs.functional.syntax._
import play.api.libs.json._
import utils.Ops.JsPathX

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

case class PostgresLabworkApplicationAtom(labwork: PostgresLabworkAtom, applicant: User, friends: Set[User], timestamp: DateTime, id: UUID) extends LabworkApplication

case class LabworkApplicationDb(labwork: UUID, applicant: UUID, friends: Set[UUID], timestamp: DateTime = DateTime.now, invalidated: Option[DateTime], id: UUID = UUID.randomUUID) extends LabworkApplication

case class LabworkApplicationFriend(labworkApplication: UUID, friend: UUID, invalidated: Option[DateTime], id: UUID) extends UniqueEntity