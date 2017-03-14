package models

import java.util.UUID

import controllers.JsonSerialisation
import org.joda.time.{DateTime, Interval, LocalDate}
import play.api.libs.json.{Json, Reads, Writes}

case class SesameSemester(label: String, abbreviation: String, start: LocalDate, end: LocalDate, examStart: LocalDate, invalidated: Option[DateTime] = None, id: UUID = SesameSemester.randomUUID) extends UniqueEntity {

  override def equals(that: scala.Any): Boolean = that match {
    case SesameSemester(l, a, s, e, ex, _, i) =>
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

object SesameSemester extends UriGenerator[SesameSemester] with JsonSerialisation[SemesterProtocol, SesameSemester, SesameSemester] {

  override implicit def reads: Reads[SemesterProtocol] = Json.reads[SemesterProtocol]

  override implicit def writes: Writes[SesameSemester] = Json.writes[SesameSemester]

  override def writesAtom: Writes[SesameSemester] = writes

  override def base: String = "semesters"

  def isCurrent(semester: SesameSemester): Boolean = {
    new Interval(semester.start.toDateTimeAtCurrentTime, semester.end.toDateTimeAtCurrentTime).containsNow
  }
}

// Postgres

case class PostgresSemester(label: String, abbreviation: String, start: LocalDate, end: LocalDate, examStart: LocalDate, id: UUID = SesameSemester.randomUUID) extends UniqueEntity

case class SemesterDb(label: String, abbreviation: String, start: LocalDate, end: LocalDate, examStart: LocalDate, invalidated: Option[DateTime] = None, id: UUID = SesameSemester.randomUUID) extends UniqueEntity {
  def toSemester = PostgresSemester(label, abbreviation, start, end, examStart, id)
}

object PostgresSemester extends JsonSerialisation[SemesterProtocol, PostgresSemester, PostgresSemester] {
  override implicit def reads: Reads[SemesterProtocol] = Json.reads[SemesterProtocol]

  override implicit def writes: Writes[PostgresSemester] = Json.writes[PostgresSemester]

  override implicit def writesAtom = writes
}