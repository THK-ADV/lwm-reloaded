package services

import models.schedule.TimetableDateEntry
import models._

object ReportCardService {

  private def toReportCardEntryType(types: Set[AssignmentEntryType]): Set[ReportCardEntryType] = {
    types.map(t => ReportCardEntryType(t.entryType, t.bool, t.int, ReportCardEntryType.randomUUID))
  }
}

trait ReportCardServiceLike {

  def reportCards(schedule: ScheduleG, assignmentPlan: AssignmentPlan): Set[ReportCard]

  def evaluate(assignmentPlan: AssignmentPlan, reportCard: ReportCard): Set[ReportCardEvaluation]
}

class ReportCardService extends ReportCardServiceLike {

  override def reportCards(schedule: ScheduleG, assignmentPlan: AssignmentPlan): Set[ReportCard] = {
    import TimetableDateEntry._
    import ReportCardService._

    val students = schedule.entries.flatMap(_.group.members)
    val assignments = assignmentPlan.entries.toVector.sortBy(_.index)

    students.foldLeft(List.empty[ReportCard]) { (list, student) =>
      val appointments = schedule.entries.filter(_.group.members.contains(student)).sortBy(toLocalDateTime)
      val entries = appointments.zip(assignments).map {
        case (se, ap) => ReportCardEntry(ap.index, ap.label, se.date, se.start, se.end, se.room, toReportCardEntryType(ap.types))
      }.toSet

      list.+:(ReportCard(student, schedule.labwork, entries))
    }.toSet
  }

  override def evaluate(assignmentPlan: AssignmentPlan, reportCard: ReportCard): Set[ReportCardEvaluation] = {
    val entries = reportCard.entries flatMap (_.entryTypes)

    def folder(reportCardEntryType: ReportCardEntryType)(f: Set[ReportCardEntryType] => (Boolean, Int)): ReportCardEvaluation = {
      val (boolRes, intRes) = f(entries.filter(_.entryType == reportCardEntryType.entryType))
      ReportCardEvaluation(reportCard.student, reportCard.labwork, reportCardEntryType.entryType, boolRes, intRes)
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
}