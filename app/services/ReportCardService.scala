package services

import models.schedule.TimetableDateEntry
import models.{AssignmentEntryType, ReportCardEntry, ReportCard, AssignmentPlan}
import scala.util.{Failure, Success, Try}

object ReportCardService {

  private def withRandomId(types: Set[AssignmentEntryType]): Set[AssignmentEntryType] = {
    types.map(t => AssignmentEntryType(t.entryType, t.bool, t.int, AssignmentEntryType.randomUUID))
  }
}

trait ReportCardServiceLike {

  def reportCards(schedule: ScheduleG, assignmentPlan: AssignmentPlan): Try[Set[ReportCard]]

  def evaluate(assignmentPlan: AssignmentPlan, reportCards: Set[ReportCard]): Unit
}

class ReportCardService extends ReportCardServiceLike {

  override def reportCards(schedule: ScheduleG, assignmentPlan: AssignmentPlan): Try[Set[ReportCard]] = {
    import TimetableDateEntry._
    import ReportCardService._

    val students = schedule.entries.flatMap(_.group.members)
    val assignments = assignmentPlan.entries.toVector.sortBy(_.index)

    students.foldLeft(Set.empty[ReportCard]) { (newSet, student) =>
      val appointments = schedule.entries.filter(_.group.members.contains(student)).sortBy(toLocalDateTime)
      val entries = appointments.zip(assignments).map {
        case (se, ap) => ReportCardEntry(ap.index, ap.label, se.date, se.start, se.end, se.room, withRandomId(ap.types))
      }.toSet

      newSet + ReportCard(student, schedule.labwork, entries)
    } match {
      case cards if cards.nonEmpty => Success(cards)
      case _ => Failure(new Throwable(s"No students found while creating report cards for ${schedule.id}"))
    }
  }

  override def evaluate(assignmentPlan: AssignmentPlan, reportCards: Set[ReportCard]): Unit = ???
}