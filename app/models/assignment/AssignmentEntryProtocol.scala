package models.assignment

import java.util.UUID

import play.api.libs.json.{Json, Reads}

case class AssignmentEntryProtocol(labwork: UUID, label: String, types: Set[AssignmentTypeProtocol], duration: Int)

case class AssignmentTypeProtocol(label: String)

object AssignmentEntryProtocol {
  implicit val reads: Reads[AssignmentEntryProtocol] = Json.reads[AssignmentEntryProtocol]
}

object AssignmentTypeProtocol {
  implicit val reads: Reads[AssignmentTypeProtocol] = Json.reads[AssignmentTypeProtocol]
}