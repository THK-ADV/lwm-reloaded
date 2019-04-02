package models

import database.helper.LdapUserStatus

import scala.collection.Seq

sealed trait Dashboard {
  def user: User
  def status: LdapUserStatus
}

case class StudentDashboard(
  user: User,
  status: LdapUserStatus,
  semester: Semester,
  labworks: Seq[LabworkLike],
  applications: Seq[LabworkApplicationLike],
  groups: Seq[GroupLike],
  cardEntries: Seq[ReportCardEntryLike],
  evaluations: Seq[ReportCardEvaluationLike],
  evaluationPatterns: Seq[ReportCardEvaluationPattern]
) extends Dashboard

case class EmployeeDashboard(
  user: User,
  status: LdapUserStatus,
  semester: Semester,
  courses: Seq[CourseAtom],
  scheduleEntries: Seq[ScheduleEntryLike]
) extends Dashboard
