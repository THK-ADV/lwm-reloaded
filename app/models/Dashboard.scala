package models

import scala.collection.Traversable

sealed trait Dashboard

case class StudentDashboard(
  semester: Semester,
  labworks: Traversable[LabworkLike],
  applications: Traversable[LabworkApplicationLike],
  groups: Traversable[GroupLike],
  cardEntries: Traversable[ReportCardEntryLike],
  evaluations: Traversable[ReportCardEvaluationLike],
  evaluationPatterns: Traversable[ReportCardEvaluationPattern]
) extends Dashboard

case class EmployeeDashboard(
  semester: Semester,
  courses: Traversable[CourseAtom],
  scheduleEntries: Traversable[ScheduleEntryLike]
) extends Dashboard
