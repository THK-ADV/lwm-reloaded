package store

import java.util.UUID

import models.{DbUser, UniqueTable}
import slick.driver.PostgresDriver.api._

trait PostgresDatabase {
  lazy val db = Database.forConfig("database")
}

class UserTable(tag: Tag) extends Table[DbUser](tag, "USERS") with UniqueTable {
  def systemId = column[String]("SYSTEM_ID")
  def lastname = column[String]("LASTNAME")
  def firstname = column[String]("FIRSTNAME")
  def email = column[String]("EMAIL")
  def registrationId = column[Option[String]]("REGISTRATION_ID")
  def enrollment = column[Option[UUID]]("ENROLLMENT")
  def status = column[String]("STATUS")

  def * = (systemId, lastname, firstname, email, status, registrationId, enrollment, id) <> ((DbUser.apply _ ).tupled, DbUser.unapply)
}


/*class DegreeTable(tag: Tag) extends Table[Degree](tag, "DEGREES") with UniqueTable {
  def label = column[String]("LABEL")
  def abbreviation = column[String]("ABBREVIATION")

  def * = (label, abbreviation, id) <> ((PostgresDegree.apply _ ).tupled, PostgresDegree.unapply)
}*/