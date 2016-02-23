package models

import java.util.UUID

import controllers.crud.JsonSerialisation
import play.api.libs.json.{Json, Reads, Writes}

case class Labwork(label: String, description: String, semester: UUID, course: UUID, degree: UUID, assignmentPlan: AssignmentPlan, id: UUID = Labwork.randomUUID) extends UniqueEntity

case class LabworkProtocol(label: String, description: String, semester: UUID, course: UUID, degree: UUID, assignmentPlan: AssignmentPlanProtocol)

object Labwork extends UriGenerator[Labwork] with JsonSerialisation[LabworkProtocol, Labwork] {
  import AssignmentPlan._
  import AssignmentEntryProtocol._
  import AssignmentEntry._

  override implicit def reads: Reads[LabworkProtocol] = Json.reads[LabworkProtocol]

  override implicit def writes: Writes[Labwork] = Json.writes[Labwork]

  override def base: String = "labworks"
}