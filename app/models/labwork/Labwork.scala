package models.labwork

import java.util.UUID

import controllers.crud.JsonSerialisation
import models.semester.Semester
import models._
import org.joda.time.DateTime
import play.api.libs.json._
import play.api.libs.functional.syntax._
import utils.Ops.JsPathX

case class Labwork(label: String, description: String, semester: UUID, course: UUID, degree: UUID, subscribable: Boolean = false, published: Boolean = false, invalidated: Option[DateTime] = None, id: UUID = Labwork.randomUUID) extends UniqueEntity

case class LabworkProtocol(label: String, description: String, semester: UUID, course: UUID, degree: UUID, subscribable: Boolean, published: Boolean)

case class LabworkAtom(label: String, description: String, semester: Semester, course: CourseAtom, degree: Degree, subscribable: Boolean, published: Boolean, invalidated: Option[DateTime] = None, id: UUID) extends UniqueEntity

object Labwork extends UriGenerator[Labwork] with JsonSerialisation[LabworkProtocol, Labwork, LabworkAtom] {

  //import Course.atomicFormat
  //import models.semester.Semester._

  override implicit def reads: Reads[LabworkProtocol] = Json.reads[LabworkProtocol]

  override implicit def writes: Writes[Labwork] = Json.writes[Labwork]

  override implicit def writesAtom: Writes[LabworkAtom] = LabworkAtom.writesAtom

  //implicit def format: Format[Labwork] = Json.format[Labwork]

  //implicit def formatAtom: Format[LabworkAtom] = Json.format[LabworkAtom]

  override def base: String = "labworks"
}

object LabworkAtom{
  implicit def writesAtom: Writes[LabworkAtom] = (
      (JsPath \ "label").write[String] and
      (JsPath \ "description").write[String] and
      (JsPath \ "semester").write[Semester] and
      (JsPath \ "course").write[CourseAtom] and
      (JsPath \ "degree").write[Degree] (Degree.writes) and
      (JsPath \ "subscribable").write[Boolean] and
      (JsPath \ "published").write[Boolean] and
      (JsPath \ "invalidated").write[Option[DateTime]] and
      (JsPath \ "id").write[UUID]
    )(unlift(LabworkAtom.unapply))
}