package database

import java.sql.Timestamp
import java.util.UUID

import models.{Course, CourseProtocol, UniqueDbEntity}
import org.joda.time.DateTime
import slick.jdbc.PostgresProfile.api._
import utils.date.DateTimeOps.DateTimeConverter

class CourseTable(tag: Tag) extends Table[CourseDb](tag, "COURSES") with UniqueTable with LabelTable with DescriptionTable with AbbreviationTable with UserIdTable {
  def semesterIndex = column[Int]("SEMESTER_INDEX")

  override def * = (label, description, abbreviation, user, semesterIndex, lastModified, invalidated, id) <> ((CourseDb.apply _).tupled, CourseDb.unapply)

  override protected def userColumnName: String = "LECTURER"
}

case class CourseDb(label: String, description: String, abbreviation: String, lecturer: UUID, semesterIndex: Int, lastModified: Timestamp = DateTime.now.timestamp, invalidated: Option[Timestamp] = None, id: UUID = UUID.randomUUID) extends UniqueDbEntity {
  override def toUniqueEntity = Course(label, description, abbreviation, lecturer, semesterIndex, id)
}