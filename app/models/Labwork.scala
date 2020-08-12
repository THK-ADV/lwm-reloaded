package models

import java.util.UUID

import play.api.libs.functional.syntax._
import play.api.libs.json._

sealed trait LabworkLike extends UniqueEntity {
  def label: String
}

case class Labwork(label: String, description: String, semester: UUID, course: UUID, degree: UUID, subscribable: Boolean = false, published: Boolean = false, id: UUID = UUID.randomUUID) extends LabworkLike

case class LabworkAtom(label: String, description: String, semester: Semester, course: CourseAtom, degree: Degree, subscribable: Boolean, published: Boolean, id: UUID) extends LabworkLike

case class LabworkProtocol(label: String, description: String, semester: UUID, course: UUID, degree: UUID, subscribable: Boolean, published: Boolean)

object Labwork {
  implicit val writes: Writes[Labwork] = Json.writes[Labwork]
}

object LabworkProtocol {
  implicit val reads: Reads[LabworkProtocol] = Json.reads[LabworkProtocol]
}

object LabworkLike {

  implicit val writes: Writes[LabworkLike] = {
    case normal: Labwork => Json.toJson(normal)(Labwork.writes)
    case atom: LabworkAtom => Json.toJson(atom)(LabworkAtom.writes)
  }
}

object LabworkAtom {

  implicit val writes: Writes[LabworkAtom] = (
    (JsPath \ "label").write[String] and
      (JsPath \ "description").write[String] and
      (JsPath \ "semester").write[Semester](Semester.writes) and
      (JsPath \ "course").write[CourseAtom](CourseAtom.writes) and
      (JsPath \ "degree").write[Degree](Degree.writes) and
      (JsPath \ "subscribable").write[Boolean] and
      (JsPath \ "published").write[Boolean] and
      (JsPath \ "id").write[UUID]
    ) (unlift(LabworkAtom.unapply))
}

