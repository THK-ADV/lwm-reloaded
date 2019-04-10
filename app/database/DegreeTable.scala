package database

import java.sql.Timestamp
import java.util.UUID

import models.{DegreeProtocol, Degree, UniqueDbEntity}
import org.joda.time.DateTime
import slick.jdbc.PostgresProfile.api._
import utils.date.DateTimeOps.DateTimeConverter

class DegreeTable(tag: Tag) extends Table[DegreeDb](tag, "DEGREES") with UniqueTable with LabelTable with AbbreviationTable {

  def labworks = TableQuery[LabworkTable].filter(_.degree === id)

  override def * = (label, abbreviation, lastModified, invalidated, id) <> ((DegreeDb.apply _).tupled, DegreeDb.unapply)
}

case class DegreeDb(label: String, abbreviation: String, lastModified: Timestamp = DateTime.now.timestamp, invalidated: Option[Timestamp] = None, id: UUID = UUID.randomUUID) extends UniqueDbEntity {
  override def toUniqueEntity = Degree(label, abbreviation, id)
}

object DegreeDb {
  def from(protocol: DegreeProtocol, existingId: Option[UUID]) = {
    DegreeDb(protocol.label, protocol.abbreviation, DateTime.now.timestamp, None, existingId.getOrElse(UUID.randomUUID))
  }
}