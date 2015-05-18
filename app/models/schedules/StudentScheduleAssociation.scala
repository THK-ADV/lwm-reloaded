package models.schedules

import java.util.UUID

import models._
import store.Namespace

case class StudentScheduleAssociation(date: String, groupScheduleAssociation: String, timetableEntry: String, id: UUID = UUID.randomUUID()) extends UniqueEntity

object StudentScheduleAssociation extends UriGenerator[StudentScheduleAssociation] {
  def generateUri(studentScheduleAssociation: StudentScheduleAssociation)(implicit ns: Namespace): String = s"${ns}studentScheduleAssociations/${studentScheduleAssociation.id}"
}