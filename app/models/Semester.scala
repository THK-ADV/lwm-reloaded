package models

import java.util.UUID

import org.joda.time.{Interval, LocalDate}
import play.api.libs.json.{Json, Reads, Writes}

case class Semester(label: String, abbreviation: String, start: LocalDate, end: LocalDate, examStart: LocalDate, id: UUID = UUID.randomUUID) extends UniqueEntity

case class SemesterProtocol(label: String, abbreviation: String, start: LocalDate, end: LocalDate, examStart: LocalDate)

object Semester {
  import utils.LwmDateTime.{writeLocalTime, writeLocalDate}

  implicit val writes: Writes[Semester] = Json.writes[Semester]

  def isCurrent(semester: Semester): Boolean = {
    new Interval(semester.start.toDateTimeAtCurrentTime, semester.end.toDateTimeAtCurrentTime).containsNow
  }
}

object SemesterProtocol {
  import utils.LwmDateTime.{readLocalTime, readLocalDate}
  implicit val reads: Reads[SemesterProtocol] = Json.reads[SemesterProtocol]
}