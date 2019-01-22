package models

sealed trait Dashboard

case class StudentDashboard(
  semester: Semester,
  labworks: Seq[LabworkLike],
  applications: Seq[LabworkApplicationLike],
  groups: Seq[GroupLike],
  cardEntries: Seq[ReportCardEntryLike],
  evaluations: Seq[ReportCardEvaluationLike],
  evaluationPatterns: Seq[ReportCardEvaluationPattern]
) extends Dashboard

case class EmployeeDashboard(
  semester: Semester,
  courses: Seq[CourseAtom],
  scheduleEntries: Seq[ScheduleEntryLike]
) extends Dashboard
