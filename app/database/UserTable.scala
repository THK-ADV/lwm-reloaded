package database

import java.sql.Timestamp
import java.util.UUID

import models.{Employee, Lecturer, Student, UniqueDbEntity, User}
import org.joda.time.DateTime
import slick.jdbc.PostgresProfile.api._
import utils.LwmDateTime.DateTimeConverter

class UserTable(tag: Tag) extends Table[UserDb](tag, "USERS") with UniqueTable {
  def systemId = column[String]("SYSTEM_ID")

  def lastname = column[String]("LASTNAME")

  def firstname = column[String]("FIRSTNAME")

  def email = column[String]("EMAIL")

  def registrationId = column[Option[String]]("REGISTRATION_ID")

  def enrollment = column[Option[UUID]]("ENROLLMENT")

  def status = column[String]("STATUS")

  def degreeFk = foreignKey("DEGREES_fkey", enrollment, TableQuery[DegreeTable])(_.id.?)

  //def labworkApplication(labwork: UUID) = TableQuery[LabworkApplicationTable].filter(lapp => lapp.applicant === id && lapp.labwork === labwork)

  override def * = (systemId, lastname, firstname, email, status, registrationId, enrollment, lastModified, invalidated, id) <> ((UserDb.apply _).tupled, UserDb.unapply)
}

case class UserDb(
  systemId: String,
  lastname: String,
  firstname: String,
  email: String,
  status: String,
  registrationId: Option[String],
  enrollment: Option[UUID],
  lastModified: Timestamp = DateTime.now.timestamp,
  invalidated: Option[Timestamp] = None,
  id: UUID = UUID.randomUUID
) extends UniqueDbEntity {

  override def toUniqueEntity: User = this match {
    case UserDb(sId, last, first, mail, stat, Some(regId), Some(enroll), _, _, studentId) if stat == User.StudentType =>
      Student(sId, last, first, mail, regId, enroll, studentId)
    case UserDb(sId, last, first, mail, stat, None, None, _, _, employeeId) if stat == User.EmployeeType =>
      Employee(sId, last, first, mail, employeeId)
    case UserDb(sId, last, first, mail, stat, None, None, _, _, lecturerId) if stat == User.LecturerType =>
      Lecturer(sId, last, first, mail, lecturerId)
  }
}
