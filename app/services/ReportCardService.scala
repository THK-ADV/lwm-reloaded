package services

import java.util.UUID

import models._
import models.genesis.ScheduleGen
import database.{ReportCardEntryDb, ReportCardEntryTypeDb, ReportCardEvaluationDb}

object ReportCardService { // TODO DI

  lazy val EvaluatedExplicit: Int = 3201 // this value indicates explicit evaluations

  def partialEval(student: UUID, labwork: UUID)(entryType: String, boolean: Boolean, int: Int): ReportCardEvaluationDb = {
    ReportCardEvaluationDb(student, labwork, entryType, boolean, int)
  }

  def evaluate(student: UUID, cards: List[ReportCardEntry], patterns: List[ReportCardEvaluationPattern], labwork: UUID): List[ReportCardEvaluationDb] = {
    val eval = partialEval(student, labwork) _

    patterns.map { pattern =>
      val counts = pattern.property match {
        case BoolBased => cards.count(_.entryTypes.exists(e => e.entryType == pattern.entryType && e.bool.getOrElse(false)))
        case IntBased => cards.filter(_.entryTypes.exists(_.entryType == pattern.entryType)).flatMap(_.entryTypes.map(_.int)).sum
      }

      eval(pattern.entryType, counts >= pattern.min, counts)
    }
  }

  def deltas(oldEvals: List[ReportCardEvaluation], newEvals: List[ReportCardEvaluationDb]): List[ReportCardEvaluationDb] = {
    newEvals.foldLeft(List.empty[ReportCardEvaluationDb]) {
      case (list, newEval) => oldEvals.find(_.label == newEval.label).fold(list.:+(newEval)) {
        case abort if abort.int == EvaluatedExplicit => list // abort when student was evaluated explicit
        case oldEval if oldEval.bool != newEval.bool || oldEval.int != newEval.int => list.:+(newEval.copy(id = oldEval.id))
        case _ => list
      }
    }
  }

  def evaluateDeltas(reportCardEntries: List[ReportCardEntryLike], patterns: List[ReportCardEvaluationPattern], existing: List[ReportCardEvaluationLike]): List[ReportCardEvaluationDb] = {
    reportCardEntries.map(_.asInstanceOf[ReportCardEntry]).groupBy(_.student).flatMap {
      case (student, cards) =>
        val newEvals = evaluate(student, cards, patterns, cards.head.labwork)
        val oldEvals = existing.map(_.asInstanceOf[ReportCardEvaluation]).filter(_.student == student)

        if (oldEvals.isEmpty) newEvals else deltas(oldEvals, newEvals)
    }.toList
  }

  def evaluate(reportCardEntries: List[ReportCardEntryLike], patterns: List[ReportCardEvaluationPattern]): List[ReportCardEvaluationDb] = {
    evaluateDeltas(reportCardEntries, patterns, List.empty)
  }

  def evaluateExplicit(student: UUID, labwork: UUID): List[ReportCardEvaluationDb] = { // TODO replace with dynamic PostgresReportCardEntryType.all when needed
    ReportCardEntryType.all.map(t => partialEval(student, labwork)(t.entryType, boolean = true, EvaluatedExplicit)).toList
  }

  def reportCards(schedule: ScheduleGen, assignmentPlan: AssignmentPlan): Vector[ReportCardEntryDb] = {
    import utils.LwmDateTime._

    val students = schedule.entries.flatMap(_.group.members).toSet
    val assignments = assignmentPlan.entries.toVector.sortBy(_.index)

    students.foldLeft(Vector.empty[ReportCardEntryDb]) { (vec, student) =>
      val appointments = schedule.entries.filter(_.group.members.contains(student)).sortBy(toLocalDateTime)

      appointments.zip(assignments).map {
        case (se, ap) =>
          val entryId = UUID.randomUUID
          val types = ap.types.map(t => ReportCardEntryTypeDb(Some(entryId), None, t.entryType))

          ReportCardEntryDb(student, assignmentPlan.labwork, ap.label, se.date.sqlDate, se.start.sqlTime, se.end.sqlTime, se.room, types, id = entryId)
      } ++ vec
    }
  }
}