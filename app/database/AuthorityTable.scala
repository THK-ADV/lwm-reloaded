package database

import java.sql.Timestamp
import java.util.UUID

import models.{Authority, UniqueDbEntity}
import org.joda.time.DateTime
import slick.jdbc.PostgresProfile.api._
import utils.date.DateTimeOps.DateTimeConverter

class AuthorityTable(tag: Tag) extends Table[AuthorityDb](tag, "AUTHORITIES") with UniqueTable with UserIdTable {
  def role = column[UUID]("ROLE")

  def course = column[Option[UUID]]("COURSE")

  def courseFk = foreignKey("COURSES_fkey", course, TableQuery[CourseTable])(_.id.?)

  def roleFk = foreignKey("ROLES_fkey", role, TableQuery[RoleTable])(_.id)

  override def * = (user, role, course, lastModified, invalidated, id) <> ((AuthorityDb.apply _).tupled, AuthorityDb.unapply)

  override protected def userColumnName: String = "USER"
}

case class AuthorityDb(user: UUID, role: UUID, course: Option[UUID] = None, lastModified: Timestamp = DateTime.now.timestamp, invalidated: Option[Timestamp] = None, id: UUID = UUID.randomUUID) extends UniqueDbEntity {
  override def toUniqueEntity = Authority(user, role, course, id)
}
