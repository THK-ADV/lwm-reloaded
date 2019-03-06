package database

import java.sql.{Date, Time, Timestamp}
import java.util.UUID

import slick.jdbc.PostgresProfile.api._
import slick.lifted.Rep
import utils.LwmDateTime._

trait UniqueTable {
  self: Table[_] =>
  def id = column[UUID]("ID", O.PrimaryKey)

  def invalidated = column[Option[Timestamp]]("INVALIDATED")

  def lastModified = column[Timestamp]("LAST_MODIFIED")

  final def isValid: Rep[Boolean] = invalidated.isEmpty

  final def lastModifiedSince(timestamp: Timestamp): Rep[Boolean] = lastModified >= timestamp
}

trait LabworkIdTable {
  self: Table[_] =>
  def labwork = column[UUID]("LABWORK")

  def labworkFk = foreignKey("LABWORKS_fkey", labwork, TableQuery[LabworkTable])(_.id)

  def memberOfCourse(course: String) = labworkFk.filter(_.course === UUID.fromString(course)).exists
}

trait RoomIdTable {
  self: Table[_] =>
  def room = column[UUID]("ROOM")

  def roomFk = foreignKey("ROOMS_fkey", room, TableQuery[RoomTable])(_.id)
}

trait TimetableIdTable {
  self: Table[_] =>
  def timetable = column[UUID]("TIMETABLE")

  def timetableFk = foreignKey("TIMETABLES_fkey", timetable, TableQuery[TimetableTable])(_.id)
}

trait ReportCardEntryIdTable {
  self: Table[_] =>
  def reportCardEntry = column[UUID]("REPORT_CARD_ENTRY")

  def reportCardEntryFk = foreignKey("REPORT_CARD_ENTRY_fkey", reportCardEntry, TableQuery[ReportCardEntryTable])(_.id)
}

trait LabelTable {
  self: Table[_] =>
  def label = column[String]("LABEL")
}

trait DescriptionTable {
  self: Table[_] =>
  def description = column[String]("DESCRIPTION")
}

trait AbbreviationTable {
  self: Table[_] =>
  def abbreviation = column[String]("ABBREVIATION")
}

trait EntryTypeTable {
  self: Table[_] =>
  def entryType = column[String]("ENTRY_TYPE")
}

trait EntryTypeLikeTable {
  self: Table[_] =>
  def bool = column[Boolean]("BOOL")

  def int = column[Int]("INT")
}

trait DateStartEndTable {
  self: Table[_] =>
  def date = column[Date]("DATE")

  def start = column[Time]("START")

  def end = column[Time]("END")

  def onDate(millis: String) = date === millis.sqlDateFromMillis

  def onStart(millis: String) = start === millis.sqlTimeFromMillis

  def onEnd(millis: String) = end === millis.sqlTimeFromMillis

  def since(millis: String) = date >= millis.sqlDateFromMillis

  def until(millis: String) = date <= millis.sqlDateFromMillis
}

trait GroupIdTable {
  self: Table[_] =>
  def group = column[UUID]("GROUP")

  def groupFk = foreignKey("GROUP_fkey", group, TableQuery[GroupTable])(_.id)
}

trait StudentIdTable {
  self: Table[_] =>
  def student = column[UUID]("STUDENT")

  def studentFk = foreignKey("STUDENTS_fkey", student, TableQuery[UserTable])(_.id)
}

trait TableFilter[T <: Table[_]] {
  def value: String

  def predicate: T => Rep[Boolean]
}