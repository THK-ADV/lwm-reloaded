package database

import java.sql.Timestamp
import java.util.UUID

import models.{Role, UniqueDbEntity}
import org.joda.time.DateTime
import slick.jdbc.PostgresProfile.api._
import utils.date.DateTimeOps.DateTimeConverter

class RoleTable(tag: Tag) extends Table[RoleDb](tag, "ROLES") with UniqueTable with LabelTable {
  override def * = (label, lastModified, invalidated, id) <> ((RoleDb.apply _).tupled, RoleDb.unapply)
}

case class RoleDb(label: String, lastModified: Timestamp = DateTime.now.timestamp, invalidated: Option[Timestamp] = None, id: UUID = UUID.randomUUID) extends UniqueDbEntity {
  def toUniqueEntity = Role(label, id)
}
