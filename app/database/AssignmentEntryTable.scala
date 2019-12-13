package database

import java.sql.Timestamp
import java.util.UUID

import models.UniqueDbEntity
import models.assignment.AssignmentEntry
import org.joda.time.DateTime
import slick.jdbc.PostgresProfile.api._
import utils.date.DateTimeOps.DateTimeConverter

class AssignmentEntryTable(tag: Tag) extends Table[AssignmentEntryDb](tag, "ASSIGNMENT_ENTRY") with UniqueTable with LabelTable with LabworkIdTable {

  def index = column[Int]("INDEX")

  def duration = column[Int]("DURATION")

  override def * = (labwork, index, label, duration, lastModified, invalidated, id) <> (mapRow, unmapRow)

  def mapRow: ((UUID, Int, String, Int, Timestamp, Option[Timestamp], UUID)) => AssignmentEntryDb = {
    case (labwork, index, label, duration, lastModified, invalidated, id) => AssignmentEntryDb(labwork, index, label, Set.empty, duration, lastModified, invalidated, id)
  }

  def unmapRow: AssignmentEntryDb => Option[(UUID, Int, String, Int, Timestamp, Option[Timestamp], UUID)] = { entry =>
    Option((entry.labwork, entry.index, entry.label, entry.duration, entry.lastModified, entry.invalidated, entry.id))
  }
}

case class AssignmentEntryDb(labwork: UUID, index: Int, label: String, types: Set[AssignmentTypeDb], duration: Int, lastModified: Timestamp = DateTime.now.timestamp, invalidated: Option[Timestamp] = None, id: UUID = UUID.randomUUID) extends UniqueDbEntity {

  import AssignmentTypeDb._

  override def toUniqueEntity = AssignmentEntry(labwork, index, label, types, duration, id)
}