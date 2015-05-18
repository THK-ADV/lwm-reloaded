package models.schedules

import java.util.UUID

import models._
import store.Namespace

case class GroupScheduleAssociation(date: String, timetableEntry: String, id: UUID = UUID.randomUUID()) extends UniqueEntity

object GroupScheduleAssociation extends UriGenerator[GroupScheduleAssociation] {
  def generateUri(groupScheduleAssociation: GroupScheduleAssociation)(implicit ns: Namespace): String = s"${ns}groupScheduleAssociations/${groupScheduleAssociation.id}"
}
