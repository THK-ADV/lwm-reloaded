package models

import java.util.UUID

import org.joda.time.DateTime
import play.api.libs.functional.syntax._
import play.api.libs.json._
import utils.LwmDateTime
import utils.Ops.JsPathX

sealed trait LabworkApplicationLike extends UniqueEntity {
  def lastModified: DateTime
}

case class LabworkApplication(labwork: UUID, applicant: UUID, friends: Set[UUID], lastModified: DateTime, id: UUID = UUID.randomUUID) extends LabworkApplicationLike

case class LabworkApplicationAtom(labwork: LabworkAtom, applicant: User, friends: Set[User], lastModified: DateTime, id: UUID) extends LabworkApplicationLike

case class LabworkApplicationProtocol(labwork: UUID, applicant: UUID, friends: Set[UUID])

object LabworkApplication {
  import utils.LwmDateTime.writeDateTime
  implicit val writes: Writes[LabworkApplication] = Json.writes[LabworkApplication]
}

object LabworkApplicationProtocol {
  implicit val reads: Reads[LabworkApplicationProtocol] = Json.reads[LabworkApplicationProtocol]
}

object LabworkApplicationLike {

  implicit val writes: Writes[LabworkApplicationLike] = {
    case normal: LabworkApplication => Json.toJson(normal)(LabworkApplication.writes)
    case atom: LabworkApplicationAtom => Json.toJson(atom)(LabworkApplicationAtom.writes)
  }
}

object LabworkApplicationAtom {

  implicit val writes: Writes[LabworkApplicationAtom] = (
    (JsPath \ "labwork").write[LabworkAtom](LabworkAtom.writes) and
      (JsPath \ "applicant").write[User] and
      (JsPath \ "friends").writeSet[User] and
      (JsPath \ "lastModified").write[DateTime](LwmDateTime.writeDateTime) and
      (JsPath \ "id").write[UUID]
    ) (unlift(LabworkApplicationAtom.unapply))
}