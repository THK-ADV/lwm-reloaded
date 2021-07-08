package database

import database.helper.LdapUserStatus
import database.helper.LdapUserStatus._
import models._
import org.joda.time.DateTime
import slick.jdbc.PostgresProfile.api._
import utils.date.DateTimeOps.DateTimeConverter

import java.sql.Timestamp
import java.util.UUID

class UserTable(tag: Tag) extends Table[UserDb](tag, "USERS") with UniqueTable {
  def systemId = column[String]("SYSTEM_ID")

  def campusId = column[String]("CAMPUS_ID")

  def lastname = column[String]("LASTNAME")

  def firstname = column[String]("FIRSTNAME")

  def email = column[String]("EMAIL")

  def registrationId = column[Option[String]]("REGISTRATION_ID")

  def enrollment = column[Option[UUID]]("ENROLLMENT")

  def status = column[String]("STATUS")

  def degreeFk =
    foreignKey("DEGREES_fkey", enrollment, TableQuery[DegreeTable])(_.id.?)

  override def * = (
    systemId,
    campusId,
    lastname,
    firstname,
    email,
    status,
    registrationId,
    enrollment,
    lastModified,
    invalidated,
    id
  ) <> (mapRow, unmapRow)

  def mapRow: (
      (
          String,
          String,
          String,
          String,
          String,
          String,
          Option[String],
          Option[UUID],
          Timestamp,
          Option[Timestamp],
          UUID
      )
  ) => UserDb = {
    case (
          systemId,
          campusId,
          lastname,
          firstname,
          email,
          status,
          registrationId,
          enrollment,
          lastModified,
          invalidated,
          id
        ) =>
      UserDb(
        systemId,
        campusId,
        lastname,
        firstname,
        email,
        LdapUserStatus(status).get,
        registrationId,
        enrollment,
        lastModified,
        invalidated,
        id
      )
  }

  def unmapRow: UserDb => Option[
    (
        String,
        String,
        String,
        String,
        String,
        String,
        Option[String],
        Option[UUID],
        Timestamp,
        Option[Timestamp],
        UUID
    )
  ] = { user =>
    Option(
      (
        user.systemId,
        user.campusId,
        user.lastname,
        user.firstname,
        user.email,
        user.status.label,
        user.registrationId,
        user.enrollment,
        user.lastModified,
        user.invalidated,
        user.id
      )
    )
  }
}

case class UserDb(
    systemId: String,
    campusId: String,
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
    case StudentStatus =>
      Student(
        systemId,
        campusId,
        lastname,
        firstname,
        email,
        registrationId.get,
        enrollment.get,
        id
      )
    case EmployeeStatus =>
      Employee(systemId, campusId, lastname, firstname, email, id)
    case LecturerStatus =>
      Lecturer(systemId, campusId, lastname, firstname, email, id)
  }
}
