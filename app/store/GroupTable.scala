package store

import java.sql.Timestamp
import java.util.UUID

import models.{Group, UniqueDbEntity, UniqueEntity}
import org.joda.time.DateTime
import slick.jdbc.PostgresProfile.api._
import utils.LwmDateTime.DateTimeConverter

class GroupTable(tag: Tag) extends Table[GroupDb](tag, "GROUP") with UniqueTable with LabworkIdTable with LabelTable {

  override def * = (label, labwork, lastModified, invalidated, id) <> (mapRow, unmapRow)

  def contains(student: UUID) = TableQuery[GroupMembershipTable].filter(m => m.group === id && m.student === student).exists

  def mapRow: ((String, UUID, Timestamp, Option[Timestamp], UUID)) => GroupDb = {
    case (label, labwork, lastModified, invalidated, id) => GroupDb(label, labwork, Set.empty, lastModified, invalidated, id)
  }

  def unmapRow: (GroupDb) => Option[(String, UUID, Timestamp, Option[Timestamp], UUID)] = { group =>
    Option((group.label, group.labwork, group.lastModified, group.invalidated, group.id))
  }
}

class GroupMembershipTable(tag: Tag) extends Table[GroupMembership](tag, "GROUP_MEMBERSHIP") with UniqueTable with GroupIdTable with StudentIdTable {
  override def * = (group, student, id) <> ((GroupMembership.apply _).tupled, GroupMembership.unapply)
}

case class GroupDb(label: String, labwork: UUID, members: Set[UUID], lastModified: Timestamp = DateTime.now.timestamp, invalidated: Option[Timestamp] = None, id: UUID = UUID.randomUUID) extends UniqueDbEntity {
  override def toUniqueEntity = Group(label, labwork, members, id)
}

case class GroupMembership(group: UUID, student: UUID, id: UUID = UUID.randomUUID) extends UniqueEntity