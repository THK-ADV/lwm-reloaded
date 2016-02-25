package models

import java.util.UUID

import controllers.crud.JsonSerialisation
import models.semester.Semester
import play.api.libs.json.{Json, Reads, Writes}

case class Labwork(label: String, description: String, semester: UUID, course: UUID, degree: UUID, assignmentPlan: AssignmentPlan, id: UUID = Labwork.randomUUID) extends UniqueEntity

case class LabworkProtocol(label: String, description: String, semester: UUID, course: UUID, degree: UUID, assignmentPlan: AssignmentPlanProtocol)

case class LabworkAtom(label: String, description: String, semester: Semester, course: Course, degree: Degree, assignmentPlan: AssignmentPlan, id: UUID)

object Labwork extends UriGenerator[Labwork] with JsonSerialisation[LabworkProtocol, Labwork] {
  import AssignmentPlan._

  override implicit def reads: Reads[LabworkProtocol] = Json.reads[LabworkProtocol]

  override implicit def writes: Writes[Labwork] = Json.writes[Labwork]

  implicit def atomicWrites: Writes[LabworkAtom] = Json.writes[LabworkAtom]

  override def base: String = "labworks"
}