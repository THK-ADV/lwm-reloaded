package database

import java.sql.Timestamp
import java.util.UUID

import models.{AssignmentEntry, AssignmentPlan, UniqueDbEntity, UniqueEntity}
import org.joda.time.DateTime
import slick.jdbc.PostgresProfile.api._
import utils.date.DateTimeOps.DateTimeConverter

class AssignmentPlanTable(tag: Tag) extends Table[AssignmentPlanDb](tag, "ASSIGNMENT_PLAN") with UniqueTable with LabworkIdTable {
  override def * = (labwork, lastModified, invalidated, id) <> (mapRow, unmapRow)

  def mapRow: ((UUID, Timestamp, Option[Timestamp], UUID)) => AssignmentPlanDb = {
    case (labwork, lastModified, invalidated, id) =>
      AssignmentPlanDb(labwork, Set.empty, lastModified, invalidated, id)
  }

  def unmapRow: AssignmentPlanDb => Option[(UUID, Timestamp, Option[Timestamp], UUID)] = { plan =>
    Option((plan.labwork, plan.lastModified, plan.invalidated, plan.id))
  }
}

class AssignmentEntryTable(tag: Tag) extends Table[AssignmentEntryDb](tag, "ASSIGNMENT_ENTRY") with UniqueTable with LabelTable {
  def assignmentPlan = column[UUID]("ASSIGNMENT_PLAN")

  def index = column[Int]("INDEX")

  def duration = column[Int]("DURATION")

  def assignmentPlanFk = foreignKey("ASSIGNMENT_PLANS_fkey", assignmentPlan, TableQuery[AssignmentPlanTable])(_.id)

  override def * = (assignmentPlan, index, label, duration, id) <> (mapRow, unmapRow)

  def mapRow: ((UUID, Int, String, Int, UUID)) => AssignmentEntryDb = {
    case (assignmentPlan, index, label, duration, id) => AssignmentEntryDb(assignmentPlan, index, label, Set.empty, duration, id)
  }

  def unmapRow: AssignmentEntryDb => Option[(UUID, Int, String, Int, UUID)] = { entry =>
    Option((entry.assignmentPlan, entry.index, entry.label, entry.duration, entry.id))
  }
}

class AssignmentEntryTypeTable(tag: Tag) extends Table[AssignmentEntryTypeDb](tag, "ASSIGNMENT_ENTRY_TYPE") with UniqueTable with EntryTypeTable with EntryTypeLikeTable {
  def assignmentEntry = column[UUID]("ASSIGNMENT_ENTRY")

  def assignmentEntryFk = foreignKey("ASSIGNMENT_ENTRIES_fkey", assignmentEntry, TableQuery[AssignmentEntryTable])(_.id)

  override def * = (assignmentEntry, entryType, bool, int, id) <> ((AssignmentEntryTypeDb.apply _).tupled, AssignmentEntryTypeDb.unapply)
}

case class AssignmentPlanDb(labwork: UUID, entries: Set[AssignmentEntry], lastModified: Timestamp = DateTime.now.timestamp, invalidated: Option[Timestamp] = None, id: UUID = UUID.randomUUID) extends UniqueDbEntity {
  override def toUniqueEntity = AssignmentPlan(labwork, entries, id)
}

case class AssignmentEntryDb(assignmentPlan: UUID, index: Int, label: String, types: Set[AssignmentEntryTypeDb], duration: Int = 1, id: UUID = UUID.randomUUID) extends UniqueEntity

case class AssignmentEntryTypeDb(assignmentEntry: UUID, entryType: String, bool: Boolean = false, int: Int = 0, id: UUID = UUID.randomUUID) extends UniqueEntity