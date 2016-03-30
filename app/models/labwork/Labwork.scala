package models.labwork

import java.util.UUID

import controllers.crud.JsonSerialisation
import models.semester.Semester
import models._
import play.api.libs.json.{Json, Reads, Writes}

case class Labwork(label: String, description: String, semester: UUID, course: UUID, degree: UUID, subscribable: Boolean = false, id: UUID = Labwork.randomUUID) extends UniqueEntity

case class LabworkProtocol(label: String, description: String, semester: UUID, course: UUID, degree: UUID, subscribable: Boolean)

case class LabworkAtom(label: String, description: String, semester: Semester, course: CourseAtom, degree: Degree, subscribable: Boolean, id: UUID)

object Labwork extends UriGenerator[Labwork] with JsonSerialisation[LabworkProtocol, Labwork] {

  import Course.atomicFormat

  override implicit def reads: Reads[LabworkProtocol] = Json.reads[LabworkProtocol]

  override implicit def writes: Writes[Labwork] = Json.writes[Labwork]

  implicit def atomicWrites: Writes[LabworkAtom] = Json.writes[LabworkAtom]

  override def base: String = "labworks"
}