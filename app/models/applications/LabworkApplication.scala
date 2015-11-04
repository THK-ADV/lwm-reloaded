package models.applications

import java.util.UUID

import controllers.crud.JsonSerialisation
import models.{UniqueEntity, UriGenerator}
import org.joda.time.DateTime
import play.api.libs.json.{Json, Reads, Writes}

case class LabworkApplication(labwork: UUID, applicant: UUID, friends: Set[UUID], timestamp: DateTime = DateTime.now(), id: UUID = LabworkApplication.randomUUID) extends UniqueEntity

case class LabworkApplicationProtocol(labwork: UUID, applicant: UUID, friends: Set[UUID])

object LabworkApplication extends UriGenerator[LabworkApplication] with JsonSerialisation[LabworkApplicationProtocol, LabworkApplication] {
  override def base: String = "labworkApplications"

  override implicit def reads: Reads[LabworkApplicationProtocol] = Json.reads[LabworkApplicationProtocol]

  override implicit def writes: Writes[LabworkApplication] = Json.writes[LabworkApplication]
}
