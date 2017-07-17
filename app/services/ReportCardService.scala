package services

import java.util.UUID

import models._

object ReportCardService {

  private def toReportCardEntryType(types: Set[SesameAssignmentEntryType]): Set[SesameReportCardEntryType] = {
    types.map(t => SesameReportCardEntryType(t.entryType))
  }
}

trait ReportCardServiceLike {

  //def reportCards(schedule: ScheduleG, assignmentPlan: SesameAssignmentPlan): Set[SesameReportCardEntry]

  def evaluate(assignmentPlan: SesameAssignmentPlan, reportCardEntries: Set[SesameReportCardEntry]): Set[SesameReportCardEvaluation]

  def evaluateExplicit(student: UUID, labwork: UUID): Set[SesameReportCardEvaluation]
}

class ReportCardService extends ReportCardServiceLike {

  /*override def reportCards(schedule: ScheduleG, assignmentPlan: SesameAssignmentPlan): Set[SesameReportCardEntry] = {
    import models.TimetableDateEntry.toLocalDateTime
    import services.ReportCardService.toReportCardEntryType
    import models.LwmDateTime.localDateTimeOrd

    val students = schedule.entries.flatMap(_.group.members).toSet
    val assignments = assignmentPlan.entries.toVector.sortBy(_.index)

    students.foldLeft(Vector.empty[SesameReportCardEntry]) { (vec, student) =>
      val appointments = schedule.entries.filter(_.group.members.contains(student)).sortBy(toLocalDateTime)
      appointments.zip(assignments).map {
        case (se, ap) => SesameReportCardEntry(student, assignmentPlan.labwork, ap.label, se.date, se.start, se.end, se.room, toReportCardEntryType(ap.types))
      } ++ vec
    }.toSet
  }*/

  override def evaluate(assignmentPlan: SesameAssignmentPlan, reportCardEntries: Set[SesameReportCardEntry]): Set[SesameReportCardEvaluation] = {
    reportCardEntries.groupBy(_.student).flatMap {
      case ((_, entries)) => evaluateStudent(assignmentPlan, entries)
    }.toSet
  }

  private def evaluateStudent(assignmentPlan: SesameAssignmentPlan, reportCardEntries: Set[SesameReportCardEntry]): Set[SesameReportCardEvaluation] = {
    val entries = reportCardEntries.flatMap(_.entryTypes)
    val student = reportCardEntries.head.student
    val labwork = reportCardEntries.head.labwork

    def prepareEval(student: UUID, labwork: UUID)(label: String, bool: Boolean, int: Int): SesameReportCardEvaluation = {
      SesameReportCardEvaluation(student, labwork, label, bool, int)
    }

    val eval = prepareEval(student, labwork) _

    def folder(reportCardEntryType: SesameReportCardEntryType)(f: Set[SesameReportCardEntryType] => (Boolean, Int)): SesameReportCardEvaluation = {
      val (boolRes, intRes) = f(entries.filter(_.entryType == reportCardEntryType.entryType))
      eval(reportCardEntryType.entryType, boolRes, intRes)
    }

    import models.SesameReportCardEntryType._
    import utils.Ops.TravOps

    Set(
      folder(Attendance)(e => (e.count(_.bool) >= assignmentPlan.attendance, 0)),
      folder(Certificate)(e => (e.count(_.bool) >= assignmentPlan.mandatory, 0)),
      folder(Bonus)(e => (true, e.foldMap(0, _.int)(_ + _))),
      folder(Supplement)(e => (e.foldMap(true, _.bool)(_ && _), 0))
    )
  }

  def evaluateExplicit(student: UUID, labwork: UUID): Set[SesameReportCardEvaluation] = {
    SesameReportCardEntryType.all.map(entryType => SesameReportCardEvaluation(student, labwork, entryType.entryType, bool = true, 0))
  }
}