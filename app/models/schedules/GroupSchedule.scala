package models.schedules

import java.util.UUID

import models._
import store.Namespace

case class GroupSchedule(id: UUID = UUID.randomUUID()) extends UniqueEntity

// not included associations: []

object GroupSchedule extends UriGenerator[GroupSchedule] {
  def generateUri(groupSchedule: GroupSchedule)(implicit ns: Namespace): String = s"${ns}groupSchedules/${groupSchedule.id}"
}
