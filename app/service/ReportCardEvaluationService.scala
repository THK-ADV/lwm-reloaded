package service

import database.ReportCardEvaluationDb
import models._
import models.helper.EvaluationProperty

import java.util.UUID

object ReportCardEvaluationService { // TODO TEST

  lazy val EvaluatedExplicit: Int = 3201 // this value indicates explicit evaluations

  def partialEval(student: UUID, labwork: UUID)(entryType: String, boolean: Boolean, int: Int): ReportCardEvaluationDb =
    ReportCardEvaluationDb(student, labwork, entryType, boolean, int)

  def count(cards: List[ReportCardEntryLike], entryType: String, property: EvaluationProperty): Int =
    property match {
      case EvaluationProperty.BoolBased =>
        cards
          .count(_.entryTypes.exists(e => e.entryType == entryType && e.bool.getOrElse(false)))
      case EvaluationProperty.IntBased =>
        cards
          .filter(_.entryTypes.exists(_.entryType == entryType))
          .flatMap(_.entryTypes.map(_.int))
          .sum
    }

  def evaluate(student: UUID, cards: List[ReportCardEntry], patterns: List[ReportCardEvaluationPattern], labwork: UUID): List[ReportCardEvaluationDb] = {
    val eval = partialEval(student, labwork) _

    patterns.map { pattern =>
      val counts = count(cards, pattern.entryType, pattern.property)
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
}
