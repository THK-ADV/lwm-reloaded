package store

import java.util.UUID

import models.{DbUser, PostgresDegree}
import slick.driver.PostgresDriver.api._
import slick.lifted.Rep

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

  def degree = foreignKey("DEGREE_fkey", enrollment, TableQuery[DegreeTable])(_.id.?, ForeignKeyAction.NoAction, ForeignKeyAction.Restrict)

  def * = (systemId, lastname, firstname, email, status, registrationId, enrollment, id) <> ((DbUser.apply _ ).tupled, DbUser.unapply)
}

class DegreeTable(tag: Tag) extends Table[PostgresDegree](tag, "DEGREES") with UniqueTable {
  def label = column[String]("LABEL")
  def abbreviation = column[String]("ABBREVIATION")

  def * = (label, abbreviation, id) <> ((PostgresDegree.apply _ ).tupled, PostgresDegree.unapply)
}