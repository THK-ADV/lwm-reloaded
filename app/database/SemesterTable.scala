package database

import java.sql.{Date, Timestamp}
import java.util.UUID

import models.{Semester, UniqueDbEntity}
import org.joda.time.DateTime
import slick.jdbc.PostgresProfile.api._
import utils.date.DateTimeOps._

class SemesterTable(tag: Tag) extends Table[SemesterDb](tag, "SEMESTERS") with UniqueTable with LabelTable with AbbreviationTable {
  def start = column[Date]("START")

  def end = column[Date]("END")

  def examStart = column[Date]("EXAM_START")

  override def * = (label, abbreviation, start, end, examStart, lastModified, invalidated, id) <> ((SemesterDb.apply _).tupled, SemesterDb.unapply)
}

case class SemesterDb(label: String, abbreviation: String, start: Date, end: Date, examStart: Date, lastModified: Timestamp = DateTime.now.timestamp, invalidated: Option[Timestamp] = None, id: UUID = UUID.randomUUID) extends UniqueDbEntity {
  import utils.date.DateTimeOps._

  override def toUniqueEntity = Semester(label, abbreviation, start.localDate, end.localDate, examStart.localDate, id)
}