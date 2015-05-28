package models.schedules

import java.util.UUID

import models._
import store.Namespace

case class StudentSchedule(id: UUID = UUID.randomUUID()) extends UniqueEntity

// not included associations: []

object StudentSchedule extends UriGenerator[StudentSchedule] {
  def generateUri(studentSchedule: StudentSchedule)(implicit ns: Namespace): String = s"${ns}studentSchedules/${studentSchedule.id}"
}