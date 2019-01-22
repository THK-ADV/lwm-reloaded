package store

import java.sql.Timestamp
import java.util.UUID

import models.{LabworkApplication, LabworkApplicationProtocol, UniqueDbEntity}
import org.joda.time.DateTime
import slick.jdbc.PostgresProfile.api._
import utils.LwmDateTime.{DateTimeConverter, SqlTimestampConverter}

class LabworkApplicationTable(tag: Tag) extends Table[LabworkApplicationDb](tag, "LABWORKAPPLICATIONS") with UniqueTable with LabworkIdTable {
  def applicant = column[UUID]("APPLICANT")

  override def * = (labwork, applicant, lastModified, invalidated, id) <> (mapRow, unmapRow)

  def mapRow: ((UUID, UUID, Timestamp, Option[Timestamp], UUID)) => LabworkApplicationDb = {
    case (labwork, applicant, lastModified, invalidated, id) =>
      LabworkApplicationDb(labwork, applicant, Set.empty, lastModified, invalidated, id)
  }

  def unmapRow: LabworkApplicationDb => Option[(UUID, UUID, Timestamp, Option[Timestamp], UUID)] = { lapp =>
    Option((lapp.labwork, lapp.applicant, lapp.lastModified, lapp.invalidated, lapp.id))
  }

  def applicantFk = foreignKey("STUDENTS_fkey", applicant, TableQuery[UserTable])(_.id)

  def joinApplicant = TableQuery[UserTable].filter(_.id === applicant)

  def friends = TableQuery[LabworkApplicationFriendTable].filter(_.labworkApplication === id).flatMap(_.friendFk)

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

  override def * = (labworkApplication, friend, lastModified, invalidated, id) <> ((LabworkApplicationFriend.apply _).tupled, LabworkApplicationFriend.unapply)
}

case class LabworkApplicationDb(labwork: UUID, applicant: UUID, friends: Set[UUID], lastModified: Timestamp = DateTime.now.timestamp, invalidated: Option[Timestamp] = None, id: UUID = UUID.randomUUID) extends UniqueDbEntity {
  override def toUniqueEntity = LabworkApplication(labwork, applicant, friends, lastModified.dateTime, id)
}

case class LabworkApplicationFriend(labworkApplication: UUID, friend: UUID, lastModified: Timestamp = DateTime.now.timestamp, invalidated: Option[Timestamp] = None, id: UUID = UUID.randomUUID) extends UniqueDbEntity {
  override def toUniqueEntity = this
}

object LabworkApplicationDb {
  def from(protocol: LabworkApplicationProtocol, existingId: Option[UUID]) = {
    LabworkApplicationDb(protocol.labwork, protocol.applicant, protocol.friends, id = existingId getOrElse UUID.randomUUID)
  }
}

