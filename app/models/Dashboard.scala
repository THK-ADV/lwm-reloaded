package models

import database.helper.LdapUserStatus

import scala.collection.Traversable

sealed trait Dashboard {
  def user: User
  def status: LdapUserStatus
}

case class StudentDashboard(
  user: User,
  status: LdapUserStatus,
  semester: Semester,
  labworks: Traversable[LabworkLike],
  applications: Traversable[LabworkApplicationLike],
  groups: Traversable[GroupLike],
  cardEntries: Traversable[ReportCardEntryLike],
  evaluations: Traversable[ReportCardEvaluationLike],
  evaluationPatterns: Traversable[ReportCardEvaluationPattern]
) extends Dashboard

case class EmployeeDashboard(
  user: User,
  status: LdapUserStatus,
  semester: Semester,
  courses: Traversable[CourseAtom],
  scheduleEntries: Traversable[ScheduleEntryLike]
) extends Dashboard
