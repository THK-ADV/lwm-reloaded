package service

import models._
import models.helper.EvaluationProperty
import org.joda.time.DateTime

import java.util.UUID

object ReportCardEvaluationService {

  lazy val FastForwardValue: Int = 3201 // this value indicates fast forward evaluations

  private def freshEval(student: UUID, labwork: UUID)(entryType: String, boolean: Boolean, int: Int): ReportCardEvaluation =
    ReportCardEvaluation(student, labwork, entryType, boolean, int, DateTime.now)

  private def evaluate(student: UUID, cards: List[ReportCardEntry], patterns: List[ReportCardEvaluationPattern], labwork: UUID): List[ReportCardEvaluation] = {
    val eval = freshEval(student, labwork) _

    patterns.map { pattern =>
      val counts = count(cards, pattern.entryType, pattern.property)
      eval(pattern.entryType, counts >= pattern.min, counts)
    }
  }

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

  def deltas(oldEvals: List[ReportCardEvaluation], newEvals: List[ReportCardEvaluation]): List[ReportCardEvaluation] =
    newEvals.foldLeft(List.empty[ReportCardEvaluation]) {
      case (list, newEval) =>
        oldEvals
          .find(_.label == newEval.label)
          .fold(list.+:(newEval)) {
            case abort if abort.int == FastForwardValue =>
              list
            case oldEval if oldEval.bool != newEval.bool || oldEval.int != newEval.int =>
              list.+:(newEval.copy(id = oldEval.id))
            case _ =>
              list
          }
    }

  def evaluateDeltas(reportCardEntries: List[ReportCardEntryLike], patterns: List[ReportCardEvaluationPattern], existing: List[ReportCardEvaluationLike]): List[ReportCardEvaluation] =
    reportCardEntries
      .map(_.asInstanceOf[ReportCardEntry])
      .groupBy(_.student)
      .flatMap {
        case (student, cards) =>
          val newEvals = evaluate(student, cards, patterns, cards.head.labwork)
          val oldEvals = existing.map(_.asInstanceOf[ReportCardEvaluation]).filter(_.student == student)

          if (oldEvals.isEmpty) newEvals else deltas(oldEvals, newEvals)
      }
      .toList

  def evaluate(reportCardEntries: List[ReportCardEntryLike], patterns: List[ReportCardEvaluationPattern]): List[ReportCardEvaluation] =
    evaluateDeltas(reportCardEntries, patterns, List.empty)

  def fastForwardStudent(student: UUID, labwork: UUID): List[ReportCardEvaluation] =
    ReportCardEntryType.all.map(t => freshEval(student, labwork)(t.entryType, boolean = true, FastForwardValue)).toList

  def fireStudent(student: UUID, labwork: UUID): List[ReportCardEvaluation] = ???
}
