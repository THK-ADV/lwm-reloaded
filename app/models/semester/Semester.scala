package models.semester

import java.util.UUID

import controllers.crud.JsonSerialisation
import models.{UniqueEntity, UriGenerator}
import org.joda.time.{DateTime, LocalDate}
import play.api.libs.json.{Json, Reads, Writes}

case class Semester(label: String, abbreviation: String, start: LocalDate, end: LocalDate, examStart: LocalDate, invalidated: Option[DateTime] = None, id: UUID = Semester.randomUUID) extends UniqueEntity {

  override def equals(that: scala.Any): Boolean = that match {
    case Semester(l, a, s, e, ex, _, i) =>
      l == label &&
      a == abbreviation &&
      s.isEqual(start) &&
      e.isEqual(end) &&
      ex.isEqual(examStart) &&
      i == id
    case _ => false
  }
}

case class SemesterProtocol(label: String, abbreviation: String, start: LocalDate, end: LocalDate, examStart: LocalDate)

object Semester extends UriGenerator[Semester] with JsonSerialisation[SemesterProtocol, Semester, Semester] {

  override implicit def reads: Reads[SemesterProtocol] = Json.reads[SemesterProtocol]

  override implicit def writes: Writes[Semester] = Json.writes[Semester]

  override def writesAtom: Writes[Semester] = writes

  override def base: String = "semesters"
}