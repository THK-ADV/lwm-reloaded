package store

import java.util.UUID

import models._
import slick.driver.PostgresDriver.api._
import slick.lifted.{QueryBase, Rep}

trait UniqueTable { self: Table[_] =>
  def id = column[UUID]("ID", O.PrimaryKey)
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

  def degreeFk = foreignKey("DEGREES_fkey", enrollment, TableQuery[DegreeTable])(_.id.?, ForeignKeyAction.NoAction, ForeignKeyAction.Restrict)

  override def * = (systemId, lastname, firstname, email, status, registrationId, enrollment, id) <> ((DbUser.apply _).tupled, DbUser.unapply)
}

class DegreeTable(tag: Tag) extends Table[PostgresDegree](tag, "DEGREES") with UniqueTable {
  def label = column[String]("LABEL")
  def abbreviation = column[String]("ABBREVIATION")

  override def * = (label, abbreviation, id) <> ((PostgresDegree.apply _).tupled, PostgresDegree.unapply)
}

class AuthorityTable(tag: Tag) extends Table[PostgresAuthority](tag, "AUTHORITIES") with UniqueTable {
  def user = column[UUID]("USER")
  def role = column[UUID]("ROLE")
  def course = column[Option[UUID]]("COURSE")

  def userFk = foreignKey("USERS_fkey", user, TableQuery[UserTable])(_.id)
  def courseFk = foreignKey("COURSES_fkey", course, TableQuery[CourseTable])(_.id.?)
  def roleFk = foreignKey("ROLES_fkey", role, TableQuery[RoleTable])(_.id)

  override def * = (user, role, course, id) <> ((PostgresAuthority.apply _).tupled, PostgresAuthority.unapply)
}

class CourseTable(tag: Tag) extends Table[PostgresCourse](tag, "COURSES") with UniqueTable {
  def label = column[String]("LABEL")
  def description = column[String]("DESCRIPTION")
  def abbreviation = column[String]("ABBREVIATION")
  def lecturer = column[UUID]("LECTURER")
  def semesterIndex = column[Int]("SEMESTER_INDEX")

  def lecturerFk = foreignKey("LECTURERS_fkey", lecturer, TableQuery[UserTable])(_.id)

  override def * = (label, description, abbreviation, lecturer, semesterIndex, id) <> ((PostgresCourse.apply _).tupled, PostgresCourse.unapply)
}

class RoleTable(tag: Tag) extends Table[PostgresRole](tag, "ROLES") with UniqueTable {
  def label = column[String]("LABEL")

  override def * = (label, id) <> (mapRow, unmapRow)

  def mapRow: ((String, UUID)) => PostgresRole = {
    case (label, id) => PostgresRole(label, Set.empty, id)
  }

  def unmapRow: (PostgresRole) => Option[(String, UUID)] = {
    role => Option((role.label, role.id))
  }

  def isLabel(label: String): Rep[Boolean] = this.label === label

  def permissions = TableQuery[RolePermissionTable].filter(_.role === id).flatMap(_.permissionFk)
}

class PermissionTable(tag: Tag) extends Table[PostgresPermission](tag, "PERMISSIONS") with UniqueTable {
  def value = column[String]("VALUE")
  def description = column[String]("DESCRIPTION")

  override def * = (value, description, id) <> ((PostgresPermission.apply _).tupled, PostgresPermission.unapply)
}

class RolePermissionTable(tag: Tag) extends Table[RolePermission](tag, "ROLE_PERMISSION") with UniqueTable {
  def role = column[UUID]("ROLE")
  def permission = column[UUID]("PERMISSION")

  def roleFk = foreignKey("ROLES_fkey", role, TableQuery[RoleTable])(_.id)
  def permissionFk = foreignKey("PERMISSIONS_fkey", permission, TableQuery[PermissionTable])(_.id)

  override def * = (role, permission, id) <> ((RolePermission.apply _).tupled, RolePermission.unapply)
}