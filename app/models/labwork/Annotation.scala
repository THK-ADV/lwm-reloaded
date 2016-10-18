package models.labwork

import java.util.UUID

import controllers.crud.JsonSerialisation
import models.users.Student
import models.{UniqueEntity, UriGenerator}
import org.joda.time.DateTime
import play.api.libs.json.{JsPath, Json, Reads, Writes}
import play.api.libs.functional.syntax._
import utils.Ops.JsPathX

case class Annotation(student: UUID, labwork: UUID, reportCardEntry: UUID, message: String, timestamp: DateTime = DateTime.now, invalidated: Option[DateTime] = None, id: UUID = Annotation.randomUUID) extends UniqueEntity {

  override def equals(that: scala.Any): Boolean = that match {
    case Annotation(s, l, r, m, t, dt, i) => s == student && l == labwork && r == reportCardEntry && m == message && t.isEqual(timestamp) && i == id
    case _ => false
  }
}

case class AnnotationProtocol(student: UUID, labwork: UUID, reportCardEntry: UUID, message: String)

case class AnnotationAtom(student: Student, labwork: Labwork, reportCardEntry: ReportCardEntry, message: String, timestamp: DateTime, invalidated: Option[DateTime], id: UUID) extends UniqueEntity

object Annotation extends UriGenerator[Annotation] with JsonSerialisation[AnnotationProtocol, Annotation, AnnotationAtom] {

  override def base: String = "annotations"

  override implicit def reads: Reads[AnnotationProtocol] = Json.reads[AnnotationProtocol]

  override implicit def writes: Writes[Annotation] = Json.writes[Annotation]

  override implicit def writesAtom: Writes[AnnotationAtom] = AnnotationAtom.writesAtom;
}

object AnnotationAtom{
  implicit def writesAtom: Writes[AnnotationAtom] = (
    (JsPath \ "student").write[Student] and
      (JsPath \ "labwork").write[Labwork] and
      (JsPath \ "reportCardEntry").write[ReportCardEntry] and
      (JsPath \ "message").write[String] and
      (JsPath \ "timestamp").write[DateTime] and
      (JsPath \ "invalidated").write[Option[DateTime]] and
      (JsPath \ "id").write[UUID]
    ) (unlift(AnnotationAtom.unapply))
}