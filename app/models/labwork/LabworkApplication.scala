package models.labwork

import java.util.UUID

import controllers.crud.JsonSerialisation
import models.users.Student
import models.{UniqueEntity, UriGenerator}
import org.joda.time.DateTime
import play.api.libs.json.{JsPath, Json, Reads, Writes}
import play.api.libs.functional.syntax._
import utils.Ops.JsPathX

case class LabworkApplication(labwork: UUID, applicant: UUID, friends: Set[UUID], timestamp: DateTime = DateTime.now, invalidated: Option[DateTime] = None, id: UUID = LabworkApplication.randomUUID) extends UniqueEntity {

  override def equals(obj: scala.Any): Boolean = obj match {
    case LabworkApplication(l, a, f, t, _, i) => l == labwork && a == applicant && f == friends && t.isEqual(timestamp) && i == id
    case _ => false
  }
}

case class LabworkApplicationProtocol(labwork: UUID, applicant: UUID, friends: Set[UUID])

case class LabworkApplicationAtom(labwork: Labwork, applicant: Student, friends: Set[Student], timestamp: DateTime, invalidated: Option[DateTime] = None, id: UUID) extends UniqueEntity

object LabworkApplication extends UriGenerator[LabworkApplication] with JsonSerialisation[LabworkApplicationProtocol, LabworkApplication, LabworkApplicationAtom] {

  override def base: String = "labworkApplications"

  override implicit def reads: Reads[LabworkApplicationProtocol] = Json.reads[LabworkApplicationProtocol]

  override implicit def writes: Writes[LabworkApplication] = Json.writes[LabworkApplication]

  override implicit def writesAtom: Writes[LabworkApplicationAtom] = LabworkApplicationAtom.writesAtom
}
object LabworkApplicationAtom{
  implicit def writesAtom: Writes[LabworkApplicationAtom] = (
    (JsPath \ "labwork").write[Labwork] and
      (JsPath \ "applicant").write[Student] and
      (JsPath \ "friends").writeSet[Student] and
      (JsPath \ "timestamp").write[DateTime] and
      (JsPath \ "invalidated").writeNullable[DateTime] and
      (JsPath \ "id").write[UUID]
    )(unlift(LabworkApplicationAtom.unapply))
}