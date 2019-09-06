package controllers.serviceResource

import java.util.UUID

import play.api.libs.json.{Json, OWrites, Reads}

case class GroupMovingRequest(labwork: UUID, student: UUID, srcGroup: UUID, destGroup: UUID)

object GroupMovingRequest {
  implicit val writes: OWrites[GroupMovingRequest] = Json.writes[GroupMovingRequest]
  implicit val reads: Reads[GroupMovingRequest] = Json.reads[GroupMovingRequest]
}
