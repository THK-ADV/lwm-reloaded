package models

import java.util.UUID

import play.api.libs.functional.syntax._
import play.api.libs.json._
import utils.Ops.JsPathX

sealed trait GroupLike extends UniqueEntity

case class Group(label: String, labwork: UUID, members: Set[UUID], id: UUID = UUID.randomUUID) extends GroupLike

case class GroupAtom(label: String, labwork: Labwork, members: Set[User], id: UUID) extends GroupLike

case class GroupProtocol(label: String, labwork: UUID, members: Set[UUID])

object GroupLike {

  implicit val writes: Writes[GroupLike] = {
    case normal: Group => Json.toJson(normal)(Group.writes)
    case atom: GroupAtom => Json.toJson(atom)(GroupAtom.writes)
  }
}

object Group {
  implicit val writes: Writes[Group] = Json.writes[Group]

  implicit val reads: Reads[Group] = Json.reads[Group]
}

object GroupProtocol {
  implicit val reads: Reads[GroupProtocol] = Json.reads[GroupProtocol]
}

object GroupAtom {

  implicit val writes: Writes[GroupAtom] = (
    (JsPath \ "label").write[String] and
      (JsPath \ "labwork").write[Labwork](Labwork.writes) and
      (JsPath \ "members").writeSet[User](User.writes) and
      (JsPath \ "id").write[UUID]
    ) (unlift(GroupAtom.unapply))
}