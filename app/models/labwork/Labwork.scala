package models.labwork

import java.util.UUID

import controllers.crud.JsonSerialisation
import models.semester.Semester
import models._
import org.joda.time.DateTime
import play.api.libs.json.{Format, Json, Reads, Writes}

case class Labwork(label: String, description: String, semester: UUID, course: UUID, degree: UUID, subscribable: Boolean = false, published: Boolean = false, invalidated: Option[DateTime] = None, id: UUID = Labwork.randomUUID) extends UniqueEntity

case class LabworkProtocol(label: String, description: String, semester: UUID, course: UUID, degree: UUID, subscribable: Boolean, published: Boolean)

case class LabworkAtom(label: String, description: String, semester: Semester, course: CourseAtom, degree: Degree, subscribable: Boolean, published: Boolean, invalidated: Option[DateTime] = None, id: UUID) extends UniqueEntity

object Labwork extends UriGenerator[Labwork] with JsonSerialisation[LabworkProtocol, Labwork, LabworkAtom] {

  import Course.atomicFormat

  override implicit def reads: Reads[LabworkProtocol] = Json.reads[LabworkProtocol]

  override implicit def writes: Writes[Labwork] = Json.writes[Labwork]

  override implicit def writesAtom: Writes[LabworkAtom] = Json.writes[LabworkAtom]

  implicit def format: Format[Labwork] = Json.format[Labwork]

  implicit def formatAtom: Format[LabworkAtom] = Json.format[LabworkAtom]

  override def base: String = "labworks"
}