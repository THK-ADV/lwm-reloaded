package services

import java.util.UUID

import models.labwork._

object ReportCardService {

  private def toReportCardEntryType(types: Set[AssignmentEntryType]): Set[ReportCardEntryType] = {
    types.map(t => ReportCardEntryType(t.entryType))
  }
}

trait ReportCardServiceLike {

  def reportCards(schedule: ScheduleG, assignmentPlan: AssignmentPlan): Set[ReportCardEntry]

  def evaluate(assignmentPlan: AssignmentPlan, reportCardEntries: Set[ReportCardEntry]): Set[ReportCardEvaluation]

  def evaluateExplicit(student: UUID, labwork: UUID): Set[ReportCardEvaluation]
}

class ReportCardService extends ReportCardServiceLike {

  override def reportCards(schedule: ScheduleG, assignmentPlan: AssignmentPlan): Set[ReportCardEntry] = {
    import models.labwork.TimetableDateEntry.toLocalDateTime
    import services.ReportCardService.toReportCardEntryType
    import models.LwmDateTime.localDateTimeOrd

    val students = schedule.entries.flatMap(_.group.members).toSet
    val assignments = assignmentPlan.entries.toVector.sortBy(_.index)

    students.foldLeft(Vector.empty[ReportCardEntry]) { (vec, student) =>
      val appointments = schedule.entries.filter(_.group.members.contains(student)).sortBy(toLocalDateTime)
      appointments.zip(assignments).map {
        case (se, ap) => ReportCardEntry(student, assignmentPlan.labwork, ap.label, se.date, se.start, se.end, se.room, toReportCardEntryType(ap.types))
      } ++ vec
    }.toSet
  }

  override def evaluate(assignmentPlan: AssignmentPlan, reportCardEntries: Set[ReportCardEntry]): Set[ReportCardEvaluation] = {
    reportCardEntries.groupBy(_.student).flatMap {
      case ((_, entries)) => evaluateStudent(assignmentPlan, entries)
    }.toSet
  }

  private def evaluateStudent(assignmentPlan: AssignmentPlan, reportCardEntries: Set[ReportCardEntry]): Set[ReportCardEvaluation] = {
    val entries = reportCardEntries.flatMap(_.entryTypes)
    val student = reportCardEntries.head.student
    val labwork = reportCardEntries.head.labwork

    def prepareEval(student: UUID, labwork: UUID)(label: String, bool: Boolean, int: Int): ReportCardEvaluation = {
      ReportCardEvaluation(student, labwork, label, bool, int)
    }

    val eval = prepareEval(student, labwork) _

    def folder(reportCardEntryType: ReportCardEntryType)(f: Set[ReportCardEntryType] => (Boolean, Int)): ReportCardEvaluation = {
      val (boolRes, intRes) = f(entries.filter(_.entryType == reportCardEntryType.entryType))
      eval(reportCardEntryType.entryType, boolRes, intRes)
    }

    import ReportCardEntryType._
    import utils.Ops.TravOps

    Set(
      folder(Attendance)(e => (e.count(_.bool) >= assignmentPlan.attendance, 0)),
      folder(Certificate)(e => (e.count(_.bool) >= assignmentPlan.mandatory, 0)),
      folder(Bonus)(e => (true, e.foldMap(0, _.int)(_ + _))),
      folder(Supplement)(e => (e.foldMap(true, _.bool)(_ && _), 0))
    )
  }

  def evaluateExplicit(student: UUID, labwork: UUID): Set[ReportCardEvaluation] = {
    ReportCardEntryType.all.map(entryType => ReportCardEvaluation(student, labwork, entryType.entryType, bool = true, 0))
  }
}