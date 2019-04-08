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
  labworkApplications: Seq[LabworkApplicationLike],
  groups: Seq[(String, LabworkLike)],
  reportCardEntries: Seq[ReportCardEntryLike],
  allEvaluations: Seq[ReportCardEvaluationLike],
  passedEvaluations: Seq[(String, String, Boolean, Int)]
) extends Dashboard

case class EmployeeDashboard(
  user: User,
  status: LdapUserStatus,
  semester: Semester,
  courses: Seq[CourseAtom],
  scheduleEntries: Seq[ScheduleEntryLike]
) extends Dashboard
