package services

import java.util.UUID

import models._

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

object ReportCardService {

  sealed trait EvaluationProperty

  case object BoolBased extends EvaluationProperty

  case object IntBased extends EvaluationProperty

  case class ReportCardEvaluationPattern(entryType: String, min: Int, property: EvaluationProperty)

  lazy val EvaluatedExplicit: Int = 3201 // this value indicates explicit evaluations

  def partialEval(student: UUID, labwork: UUID)(entryType: String, boolean: Boolean, int: Int): ReportCardEvaluationDb = {
    ReportCardEvaluationDb(student, labwork, entryType, boolean, int)
  }

  def evaluate(student: UUID, cards: List[PostgresReportCardEntry], patterns: List[ReportCardEvaluationPattern], labwork: UUID): List[ReportCardEvaluationDb] = {
    val eval = partialEval(student, labwork) _

    patterns.map { pattern =>
      val counts = pattern.property match {
        case BoolBased => cards.count(_.entryTypes.exists(e => e.entryType == pattern.entryType && e.bool.getOrElse(false)))
        case IntBased => cards.filter(_.entryTypes.exists(_.entryType == pattern.entryType)).flatMap(_.entryTypes.map(_.int)).sum
      }

      eval(pattern.entryType, counts >= pattern.min, counts)
    }
  }

  def deltas(oldEvals: List[PostgresReportCardEvaluation], newEvals: List[ReportCardEvaluationDb]): List[ReportCardEvaluationDb] = {
    newEvals.foldLeft(List.empty[ReportCardEvaluationDb]) {
      case (list, newEval) => oldEvals.find(_.label == newEval.label).fold(list.:+(newEval)) {
        case abort if abort.int == EvaluatedExplicit => list // abort when student was evaluated explicit
        case oldEval if oldEval.bool != newEval.bool || oldEval.int != newEval.int => list.:+(newEval.copy(id = oldEval.id))
        case _ => list
      }
    }
  }

  def evaluateDeltas(reportCardEntries: List[ReportCardEntry], patterns: List[ReportCardEvaluationPattern], existing: List[ReportCardEvaluation]): List[ReportCardEvaluationDb] = {
    reportCardEntries.map(_.asInstanceOf[PostgresReportCardEntry]).groupBy(_.student).flatMap {
      case (student, cards) =>
        val newEvals = evaluate(student, cards, patterns, cards.head.labwork)
        val oldEvals = existing.map(_.asInstanceOf[PostgresReportCardEvaluation]).filter(_.student == student)

        if (oldEvals.isEmpty) newEvals else deltas(oldEvals, newEvals)
    }.toList
  }

  def evaluate(reportCardEntries: List[ReportCardEntry], patterns: List[ReportCardEvaluationPattern]): List[ReportCardEvaluationDb] = {
    evaluateDeltas(reportCardEntries, patterns, List.empty)
  }

  def evaluateExplicit(student: UUID, labwork: UUID): List[ReportCardEvaluationDb] = { // TODO replace with dynamic PostgresReportCardEntryType.all when needed
    PostgresReportCardEntryType.all.map(t => partialEval(student, labwork)(t.entryType, boolean = true, EvaluatedExplicit)).toList
  }

  def reportCards(schedule: ScheduleGen, assignmentPlan: PostgresAssignmentPlan): Vector[ReportCardEntryDb] = {
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