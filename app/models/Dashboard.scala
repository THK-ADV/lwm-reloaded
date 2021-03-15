package models

import database.helper.LdapUserStatus
import service.dashboard.{DashboardEvaluationResult, DashboardGroupLabel}

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
  groups: Seq[DashboardGroupLabel],
  reportCardEntries: Seq[(ReportCardEntryLike, Set[ReportCardRescheduledLike])],
  evaluationResults: Seq[DashboardEvaluationResult],
  scheduleEntries: Seq[ScheduleEntryLike]
) extends Dashboard

case class EmployeeDashboard(
  user: User,
  status: LdapUserStatus,
  semester: Semester,
  courses: Seq[CourseAtom],
  scheduleEntries: Seq[ScheduleEntryLike]
) extends Dashboard
