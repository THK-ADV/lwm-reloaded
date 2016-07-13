package models.labwork

import java.util.UUID

import controllers.crud.JsonSerialisation
import models.users.Student
import models.{UriGenerator, UniqueEntity}
import org.joda.time.DateTime
import play.api.libs.json.{Json, Reads, Writes}

case class Annotation(student: UUID, labwork: UUID, reportCardEntry: UUID, message: String, timestamp: DateTime = DateTime.now, id: UUID = Annotation.randomUUID) extends UniqueEntity {

  override def equals(that: scala.Any): Boolean = that match {
    case Annotation(s, l, r, m, t, i) => s == student && l == labwork && r == reportCardEntry && m == message && t.isEqual(timestamp) && i == id
    case _ => false
  }
}

case class AnnotationProtocol(student: UUID, labwork: UUID, reportCardEntry: UUID, message: String)

case class AnnotationAtom(student: Student, labwork: Labwork, reportCardEntry: ReportCardEntry, message: String, timestamp: DateTime, id: UUID) extends UniqueEntity

object Annotation extends UriGenerator[Annotation] with JsonSerialisation[AnnotationProtocol, Annotation, AnnotationAtom] {

  override def base: String = "annotations"

  override implicit def reads: Reads[AnnotationProtocol] = Json.reads[AnnotationProtocol]

  override implicit def writes: Writes[Annotation] = Json.writes[Annotation]

  override implicit def writesAtom: Writes[AnnotationAtom] = Json.writes[AnnotationAtom]
}