package database

import java.sql.Timestamp
import java.util.UUID

import database.helper.LdapUserStatus
import database.helper.LdapUserStatus._
import models.{Employee, Lecturer, Student, UniqueDbEntity, User}
import org.joda.time.DateTime
import slick.jdbc.PostgresProfile.api._
import utils.date.DateTimeOps.DateTimeConverter

class UserTable(tag: Tag) extends Table[UserDb](tag, "USERS") with UniqueTable {
  def systemId = column[String]("SYSTEM_ID")

  def lastname = column[String]("LASTNAME")

  def firstname = column[String]("FIRSTNAME")

  def email = column[String]("EMAIL")

  def registrationId = column[Option[String]]("REGISTRATION_ID")

  def enrollment = column[Option[UUID]]("ENROLLMENT")

  def status = column[String]("STATUS")

  def degreeFk = foreignKey("DEGREES_fkey", enrollment, TableQuery[DegreeTable])(_.id.?)

  override def * = (systemId, lastname, firstname, email, status, registrationId, enrollment, lastModified, invalidated, id) <> (mapRow, unmapRow)

  def mapRow: ((String, String, String, String, String, Option[String], Option[UUID], Timestamp, Option[Timestamp], UUID)) => UserDb = {
    case (systemId, lastname, firstname, email, status, registrationId, enrollment, lastModified, invalidated, id) =>
      UserDb(systemId, lastname, firstname, email, LdapUserStatus(status).get, registrationId, enrollment, lastModified, invalidated, id)
  }

  def unmapRow: UserDb => Option[(String, String, String, String, String, Option[String], Option[UUID], Timestamp, Option[Timestamp], UUID)] = { user =>
    Option((user.systemId, user.lastname, user.firstname, user.email, user.status.label, user.registrationId, user.enrollment, user.lastModified, user.invalidated, user.id))
  }
}

case class UserDb(
  systemId: String,
  lastname: String,
  firstname: String,
  email: String,
  status: LdapUserStatus,
  registrationId: Option[String],
  enrollment: Option[UUID],
  lastModified: Timestamp = DateTime.now.timestamp,
  invalidated: Option[Timestamp] = None,
  id: UUID = UUID.randomUUID
) extends UniqueDbEntity {

  override def toUniqueEntity: User = status match {
    case StudentStatus => Student(systemId, lastname, firstname, email, registrationId.get, enrollment.get, id)
    case EmployeeStatus => Employee(systemId, lastname, firstname, email, id)
    case LecturerStatus => Lecturer(systemId, lastname, firstname, email, id)
  }
}
