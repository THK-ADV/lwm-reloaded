package store

import java.util.UUID

import models._
import org.joda.time.{DateTime, LocalDate}
import org.joda.time.format.ISODateTimeFormat
import slick.driver.PostgresDriver.api._
import slick.lifted.{QueryBase, Rep}

trait UniqueTable { self: Table[_] =>
  implicit val jodaDateTime = MappedColumnType.base[DateTime, String](ISODateTimeFormat.dateTime.print, DateTime.parse)
  implicit val jodaLocalDate = MappedColumnType.base[LocalDate, String](d => d.toString, LocalDate.parse) // LocalDate.toString formats to ISO8601 (yyyy-MM-dd)

  def id = column[UUID]("ID", O.PrimaryKey)
  def invalidated = column[Option[DateTime]]("INVALIDATED")
}

trait LabworkTableId { self: Table[_] =>
  def labwork = column[UUID]("LABWORK")
}

trait LabelTable { self: Table[_] =>
  def label = column[String]("LABEL")
}

trait DescriptionTable { self: Table[_] =>
  def description = column[String]("DESCRIPTION")
}

trait AbbreviationTable { self: Table[_] =>
  def abbreviation = column[String]("ABBREVIATION")
}

trait PostgresDatabase {
  lazy val db = Database.forConfig("database")
}

trait TableFilter[T <: Table[_]] {
  def value: String

  def predicate: T => Rep[Boolean]
}

class UserTable(tag: Tag) extends Table[DbUser](tag, "USERS") with UniqueTable {
  def systemId = column[String]("SYSTEM_ID")
  def lastname = column[String]("LASTNAME")
  def firstname = column[String]("FIRSTNAME")
  def email = column[String]("EMAIL")
  def registrationId = column[Option[String]]("REGISTRATION_ID")
  def enrollment = column[Option[UUID]]("ENROLLMENT")
  def status = column[String]("STATUS")

  def degreeFk = foreignKey("DEGREES_fkey", enrollment, TableQuery[DegreeTable])(_.id.?)

  def labworkApplication(labwork: UUID) = TableQuery[LabworkApplicationTable].filter(lapp => lapp.applicant === id && lapp.labwork === labwork)

  override def * = (systemId, lastname, firstname, email, status, registrationId, enrollment, invalidated, id) <> ((DbUser.apply _).tupled, DbUser.unapply)
}

class DegreeTable(tag: Tag) extends Table[DegreeDb](tag, "DEGREES") with UniqueTable with LabelTable with AbbreviationTable {

  def labworks =  TableQuery[LabworkTable].filter(_.degree === id)

  override def * = (label, abbreviation, invalidated, id) <> ((DegreeDb.apply _).tupled, DegreeDb.unapply)
}

class AuthorityTable(tag: Tag) extends Table[AuthorityDb](tag, "AUTHORITIES") with UniqueTable {
  def user = column[UUID]("USER")
  def role = column[UUID]("ROLE")
  def course = column[Option[UUID]]("COURSE")

  def userFk = foreignKey("USERS_fkey", user, TableQuery[UserTable])(_.id)
  def courseFk = foreignKey("COURSES_fkey", course, TableQuery[CourseTable])(_.id.?)
  def roleFk = foreignKey("ROLES_fkey", role, TableQuery[RoleTable])(_.id)

  override def * = (user, role, course, invalidated, id) <> ((AuthorityDb.apply _).tupled, AuthorityDb.unapply)
}
class SemesterTable(tag: Tag) extends Table[SemesterDb](tag, "SEMESTERS") with UniqueTable with LabelTable with AbbreviationTable {
  def start = column[LocalDate]("START")
  def end = column[LocalDate]("END")
  def examStart = column[LocalDate]("EXAM_START")

  override def * = (label, abbreviation, start, end, examStart, invalidated, id) <> ((SemesterDb.apply _).tupled, SemesterDb.unapply)
}

class CourseTable(tag: Tag) extends Table[CourseDb](tag, "COURSES") with UniqueTable with LabelTable with DescriptionTable with AbbreviationTable {
  def lecturer = column[UUID]("LECTURER")
  def semesterIndex = column[Int]("SEMESTER_INDEX")

  def lecturerFk = foreignKey("LECTURERS_fkey", lecturer, TableQuery[UserTable])(_.id)

  def joinLecturer = TableQuery[UserTable].filter(_.id === lecturer)

  override def * = (label, description, abbreviation, lecturer, semesterIndex, invalidated, id) <> ((CourseDb.apply _).tupled, CourseDb.unapply)
}

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

  override def * = (label, description, semester, course, degree, subscribable, published, invalidated, id) <> ((LabworkDb.apply _).tupled, LabworkDb.unapply)
}

class LabworkApplicationTable(tag: Tag) extends Table[LabworkApplicationDb](tag, "LABWORKAPPLICATIONS") with UniqueTable with LabworkTableId {
  def applicant = column[UUID]("APPLICANT")
  def timestamp = column[DateTime]("TIMESTAMP")

  override def * = (labwork, applicant, timestamp, invalidated, id) <> (mapRow, unmapRow)

  def mapRow: ((UUID, UUID, DateTime, Option[DateTime], UUID)) => LabworkApplicationDb = {
    case (labwork, applicant, timestamp, invalidated, id) => LabworkApplicationDb(labwork, applicant, Set.empty, timestamp, invalidated, id)
  }

  def unmapRow: (LabworkApplicationDb) => Option[(UUID, UUID, DateTime, Option[DateTime], UUID)] = {
    lapp => Option((lapp.labwork, lapp.applicant, lapp.timestamp, lapp.invalidated, lapp.id))
  }

  def friends = TableQuery[LabworkApplicationFriendTable].filter(_.labworkApplication === id).flatMap(_.friendFk)
  def joinLabwork = TableQuery[LabworkTable].filter(_.id === labwork)
  def joinApplicant = TableQuery[UserTable].filter(_.id === applicant)

  def fullJoin = {
    for {
      f <- friends
      l <- joinLabwork
      a <- joinApplicant
    } yield (f, a, l)
  }
}

class LabworkApplicationFriendTable(tag: Tag) extends Table[LabworkApplicationFriend](tag, "LABWORKAPPLICATION_FRIEND") with UniqueTable {
  def labworkApplication = column[UUID]("LABWORKAPPLICATION")
  def friend = column[UUID]("FRIEND")

  def labworkApplicationFk = foreignKey("LABWORKAPPLICATIONS_fkey", labworkApplication, TableQuery[LabworkApplicationTable])(_.id)
  def friendFk = foreignKey("USERS_fkey", friend, TableQuery[UserTable])(_.id)

  override def * = (labworkApplication, friend, invalidated, id) <> ((LabworkApplicationFriend.apply _).tupled, LabworkApplicationFriend.unapply)
}

class RoleTable(tag: Tag) extends Table[RoleDb](tag, "ROLES") with UniqueTable with LabelTable {
  override def * = (label, invalidated, id) <> (mapRow, unmapRow)

  def mapRow: ((String, Option[DateTime], UUID)) => RoleDb = {
    case (label, invalidated, id) => RoleDb(label, Set.empty, invalidated, id)
  }

  def unmapRow: (RoleDb) => Option[(String, Option[DateTime], UUID)] = {
    role => Option((role.label, role.invalidated, role.id))
  }

  def isLabel(label: String): Rep[Boolean] = this.label === label

  def permissions = TableQuery[RolePermissionTable].filter(_.role === id).flatMap(_.permissionFk)
}

class PermissionTable(tag: Tag) extends Table[PermissionDb](tag, "PERMISSIONS") with UniqueTable with DescriptionTable {
  def value = column[String]("VALUE")

  override def * = (value, description, invalidated, id) <> ((PermissionDb.apply _).tupled, PermissionDb.unapply)
}

class RolePermissionTable(tag: Tag) extends Table[RolePermission](tag, "ROLE_PERMISSION") with UniqueTable {
  def role = column[UUID]("ROLE")
  def permission = column[UUID]("PERMISSION")

  def roleFk = foreignKey("ROLES_fkey", role, TableQuery[RoleTable])(_.id)
  def permissionFk = foreignKey("PERMISSIONS_fkey", permission, TableQuery[PermissionTable])(_.id)

  override def * = (role, permission, invalidated, id) <> ((RolePermission.apply _).tupled, RolePermission.unapply)
}