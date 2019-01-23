package database

import java.sql.Timestamp
import java.util.UUID

import models.{Course, CourseProtocol, UniqueDbEntity}
import org.joda.time.DateTime
import slick.jdbc.PostgresProfile.api._
import utils.LwmDateTime.DateTimeConverter

class CourseTable(tag: Tag) extends Table[CourseDb](tag, "COURSES") with UniqueTable with LabelTable with DescriptionTable with AbbreviationTable {
  def lecturer = column[UUID]("LECTURER")

  def semesterIndex = column[Int]("SEMESTER_INDEX")

  def lecturerFk = foreignKey("LECTURERS_fkey", lecturer, TableQuery[UserTable])(_.id)

  def joinLecturer = TableQuery[UserTable].filter(_.id === lecturer)

  override def * = (label, description, abbreviation, lecturer, semesterIndex, lastModified, invalidated, id) <> ((CourseDb.apply _).tupled, CourseDb.unapply)
}

case class CourseDb(label: String, description: String, abbreviation: String, lecturer: UUID, semesterIndex: Int, lastModified: Timestamp = DateTime.now.timestamp, invalidated: Option[Timestamp] = None, id: UUID = UUID.randomUUID) extends UniqueDbEntity {
  override def toUniqueEntity = Course(label, description, abbreviation, lecturer, semesterIndex, id)
}

object CourseDb {
  def from(protocol: CourseProtocol, existingId: Option[UUID]) = {
    CourseDb(protocol.label, protocol.description, protocol.abbreviation, protocol.lecturer, protocol.semesterIndex, DateTime.now.timestamp, None, existingId.getOrElse(UUID.randomUUID))
  }
}