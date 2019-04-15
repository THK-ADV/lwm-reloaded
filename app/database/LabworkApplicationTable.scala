package database

import java.sql.Timestamp
import java.util.UUID

import models.{LabworkApplication, LabworkApplicationProtocol, UniqueDbEntity}
import org.joda.time.DateTime
import slick.jdbc.PostgresProfile.api._
import utils.date.DateTimeOps.{DateTimeConverter, SqlTimestampConverter}

class LabworkApplicationTable(tag: Tag) extends Table[LabworkApplicationDb](tag, "LABWORKAPPLICATIONS") with UniqueTable with LabworkIdTable with UserIdTable {

  override def * = (labwork, user, lastModified, invalidated, id) <> (mapRow, unmapRow)

  def mapRow: ((UUID, UUID, Timestamp, Option[Timestamp], UUID)) => LabworkApplicationDb = {
    case (labwork, applicant, lastModified, invalidated, id) =>
      LabworkApplicationDb(labwork, applicant, Set.empty, lastModified, invalidated, id)
  }

  def unmapRow: LabworkApplicationDb => Option[(UUID, UUID, Timestamp, Option[Timestamp], UUID)] = { lapp =>
    Option((lapp.labwork, lapp.applicant, lapp.lastModified, lapp.invalidated, lapp.id))
  }

  def friends = TableQuery[LabworkApplicationFriendTable].filter(_.labworkApplication === id).flatMap(_.userFk)

  override protected def userColumnName: String = "APPLICANT"
}

class LabworkApplicationFriendTable(tag: Tag) extends Table[LabworkApplicationFriend](tag, "LABWORKAPPLICATION_FRIEND") with UniqueTable with UserIdTable {
  def labworkApplication = column[UUID]("LABWORKAPPLICATION")

  def labworkApplicationFk = foreignKey("LABWORKAPPLICATIONS_fkey", labworkApplication, TableQuery[LabworkApplicationTable])(_.id)

  override def * = (labworkApplication, user, lastModified, invalidated, id) <> ((LabworkApplicationFriend.apply _).tupled, LabworkApplicationFriend.unapply)

  override protected def userColumnName: String = "FRIEND"
}

case class LabworkApplicationDb(labwork: UUID, applicant: UUID, friends: Set[UUID], lastModified: Timestamp = DateTime.now.timestamp, invalidated: Option[Timestamp] = None, id: UUID = UUID.randomUUID) extends UniqueDbEntity {
  override def toUniqueEntity = LabworkApplication(labwork, applicant, friends, lastModified.dateTime, id)
}

case class LabworkApplicationFriend(labworkApplication: UUID, friend: UUID, lastModified: Timestamp = DateTime.now.timestamp, invalidated: Option[Timestamp] = None, id: UUID = UUID.randomUUID) extends UniqueDbEntity {
  override def toUniqueEntity = this
}

