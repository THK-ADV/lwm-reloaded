package database

import java.util.UUID

import models.UniqueEntity
import slick.jdbc.PostgresProfile.api._

class AssignmentTypeTable(tag: Tag) extends Table[AssignmentTypeDb](tag, "ASSIGNMENT_TYPE") with UniqueTable with LabelTable {
  def assignmentEntry = column[UUID]("ASSIGNMENT_ENTRY")

  def assignmentEntryFk = foreignKey("ASSIGNMENT_ENTRIES_fkey", assignmentEntry, TableQuery[AssignmentEntryTable])(_.id)

  override def * = (assignmentEntry, label, id) <> ((AssignmentTypeDb.apply _).tupled, AssignmentTypeDb.unapply)
}

case class AssignmentTypeDb(assignmentEntry: UUID, label: String, id: UUID) extends UniqueEntity
