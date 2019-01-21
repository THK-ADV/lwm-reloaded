package models

import models.{Group, Labwork, LabworkApplication, PostgresCourseAtom, PostgresSemester, ReportCardEntry, ReportCardEvaluation, ReportCardEvaluationPattern, ScheduleEntry}

sealed trait Dashboard

case class StudentDashboard(
  semester: PostgresSemester,
  labworks: Seq[Labwork],
  applications: Seq[LabworkApplication],
  groups: Seq[Group],
  cardEntries: Seq[ReportCardEntry],
  evaluations: Seq[ReportCardEvaluation],
  evaluationPatterns: Seq[ReportCardEvaluationPattern]
) extends Dashboard

case class EmployeeDashboard(
  semester: PostgresSemester,
  courses: Seq[PostgresCourseAtom],
  scheduleEntries: Seq[ScheduleEntry]
) extends Dashboard
