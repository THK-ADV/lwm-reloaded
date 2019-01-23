package database

import java.sql.Timestamp
import java.util.UUID

import models.{Labwork, LabworkProtocol, UniqueDbEntity}
import org.joda.time.DateTime
import slick.jdbc.PostgresProfile.api._
import utils.LwmDateTime.DateTimeConverter

class LabworkTable(tag: Tag) extends Table[LabworkDb](tag, "LABWORK") with UniqueTable with LabelTable with DescriptionTable {
  def semester = column[UUID]("SEMESTER")

  def course = column[UUID]("COURSE")

  def degree = column[UUID]("DEGREE")

  def subscribable = column[Boolean]("SUBSCRIBABLE")

  def published = column[Boolean]("PUBLISHED")

  def semesterFk = foreignKey("SEMESTERS_fkey", semester, TableQuery[SemesterTable])(_.id)

  def courseFk = foreignKey("COURSES_fkey", course, TableQuery[CourseTable])(_.id)

  def degreeFk = foreignKey("DEGREES_fkey", degree, TableQuery[DegreeTable])(_.id)

  def joinCourse = TableQuery[CourseTable].filter(_.id === course)

  def joinDegree = TableQuery[DegreeTable].filter(_.id === degree)

  def joinSemester = TableQuery[SemesterTable].filter(_.id === semester)

  def fullJoin = {
    for {
      c <- joinCourse
      d <- joinDegree
      s <- joinSemester
    } yield (c, d, s)
  }

  override def * = (label, description, semester, course, degree, subscribable, published, lastModified, invalidated, id) <> ((LabworkDb.apply _).tupled, LabworkDb.unapply)
}

case class LabworkDb(label: String, description: String, semester: UUID, course: UUID, degree: UUID, subscribable: Boolean = false, published: Boolean = false, lastModified: Timestamp = DateTime.now.timestamp, invalidated: Option[Timestamp] = None, id: UUID = UUID.randomUUID) extends UniqueDbEntity {
  override def toUniqueEntity = Labwork(label, description, semester, course, degree, subscribable, published, id)
}

object LabworkDb {
  def from(protocol: LabworkProtocol, existingId: Option[UUID]): LabworkDb = {
    LabworkDb(protocol.label, protocol.description, protocol.semester, protocol.course, protocol.degree, protocol.subscribable, protocol.published, DateTime.now.timestamp, None, existingId getOrElse UUID.randomUUID)
  }
}