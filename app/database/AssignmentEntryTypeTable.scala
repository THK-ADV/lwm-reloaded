package database

import java.util.UUID

import models.UniqueEntity
import slick.jdbc.PostgresProfile.api._

class AssignmentEntryTypeTable(tag: Tag) extends Table[AssignmentEntryTypeDb](tag, "ASSIGNMENT_ENTRY_TYPE") with UniqueTable with EntryTypeTable {
  def assignmentEntry = column[UUID]("ASSIGNMENT_ENTRY")

  def assignmentEntryFk = foreignKey("ASSIGNMENT_ENTRIES_fkey", assignmentEntry, TableQuery[AssignmentEntryTable])(_.id)

  override def * = (assignmentEntry, entryType, id) <> ((AssignmentEntryTypeDb.apply _).tupled, AssignmentEntryTypeDb.unapply)
}

case class AssignmentEntryTypeDb(assignmentEntry: UUID, entryType: String, id: UUID = UUID.randomUUID) extends UniqueEntity
