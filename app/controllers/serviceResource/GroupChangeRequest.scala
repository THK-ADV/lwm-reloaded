package controllers.serviceResource

import java.util.UUID

import play.api.libs.json.{Json, OWrites, Reads}

case class GroupChangeRequest(labwork: UUID, student: UUID, group: UUID)

object GroupChangeRequest {
  implicit val writes: OWrites[GroupChangeRequest] = Json.writes[GroupChangeRequest]

  implicit val reads: Reads[GroupChangeRequest] = Json.reads[GroupChangeRequest]
}
