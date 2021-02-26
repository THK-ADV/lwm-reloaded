package service

import base.TestBaseDefinition
import models.ReportCardEntryType._
import models.helper.EvaluationProperty._
import models.{ReportCardEntry, ReportCardEntryType, ReportCardEvaluation, ReportCardEvaluationPattern}
import org.joda.time.{DateTime, LocalDate, LocalTime}
import org.scalatest.WordSpec

import java.util.UUID

class ReportCardEvaluationServiceSpec extends WordSpec with TestBaseDefinition {

  import service.ReportCardEvaluationService._

  def reportCardEntry(student: UUID, labwork: UUID)(index: Int, types: Set[ReportCardEntryType]) =
    ReportCardEntry(student, labwork, index.toString, LocalDate.now, LocalTime.now, LocalTime.now, UUID.randomUUID, types, index)

  def att(bool: Option[Boolean]): ReportCardEntryType =
    Attendance.copy(bool = bool)

  def cert(bool: Option[Boolean]): ReportCardEntryType =
    Certificate.copy(bool = bool)

  def supp(bool: Option[Boolean]): ReportCardEntryType =
    Supplement.copy(bool = bool)

  def bonus(int: Int): ReportCardEntryType =
    Bonus.copy(int = int)

  def eval(student: UUID, labwork: UUID)(label: String, bool: Boolean, int: Int) =
    ReportCardEvaluation(student, labwork, label, bool, int, DateTime.now)

  "A ReportCardEvaluationServiceSpec" should {
    "fast forward an evaluation by setting all bools to true" in {
      val student = UUID.randomUUID
      val labwork = UUID.randomUUID

      val evals = fastForwardStudent(student, labwork)

      evals.map(_.label).toSet shouldBe ReportCardEntryType.all.map(_.entryType)
      evals foreach { eval =>
        eval.student shouldBe student
        eval.labwork shouldBe labwork
        eval.int shouldBe FastForwardValue
        eval.bool shouldBe true
      }
    }
  }

  "fast rewind an evaluation by setting all bools to false" in {
    val student = UUID.randomUUID
    val labwork = UUID.randomUUID

    val evals = fireStudent(student, labwork)

    evals.map(_.label).toSet shouldBe ReportCardEntryType.all.map(_.entryType)
    evals foreach { eval =>
      eval.student shouldBe student
      eval.labwork shouldBe labwork
      eval.int shouldBe FireValue
      eval.bool shouldBe false
    }
  }

  "count number of bools and ints of entryTypes properly" in {
    val student = UUID.randomUUID
    val labwork = UUID.randomUUID
    val freshReportCard = reportCardEntry(student, labwork) _

    val cardEntries = List(
      freshReportCard(0, Set(att(Some(true)), cert(Some(true)))),
      freshReportCard(1, Set(att(Some(true)), cert(Some(false)))),
      freshReportCard(2, Set(att(Some(true)), cert(Some(true)), bonus(10))),
      freshReportCard(3, Set(att(Some(true)), bonus(5))),
    )

    count(cardEntries, Attendance.entryType, BoolBased) shouldBe 4
    count(cardEntries, Certificate.entryType, BoolBased) shouldBe 2
    count(cardEntries, Bonus.entryType, IntBased) shouldBe 15
    count(cardEntries, Supplement.entryType, BoolBased) shouldBe 0
  }

  "create positive evaluations results when everything is fine" in {
    val student = UUID.randomUUID
    val labwork = UUID.randomUUID
    val freshReportCard = reportCardEntry(student, labwork) _

    val cardEntries = List(
      freshReportCard(0, Set(att(Some(true)), cert(Some(true)))),
      freshReportCard(1, Set(att(Some(true)), cert(Some(true)))),
      freshReportCard(2, Set(att(Some(true)), cert(Some(true)), bonus(10))),
    )

    val patterns = List(
      ReportCardEvaluationPattern(labwork, Attendance.entryType, 3, BoolBased),
      ReportCardEvaluationPattern(labwork, Certificate.entryType, 3, BoolBased),
    )

    val result = evaluate(cardEntries, patterns)

    result.size shouldBe patterns.size
    result foreach { r =>
      r.bool shouldBe true
      r.int shouldBe 3
      patterns.exists(_.entryType == r.label) shouldBe true
    }
  }

  "create negative evaluations results if something is missing" in {
    val student = UUID.randomUUID
    val labwork = UUID.randomUUID
    val freshReportCard = reportCardEntry(student, labwork) _

    val cardEntries = List(
      freshReportCard(0, Set(att(Some(true)), cert(Some(true)))),
      freshReportCard(1, Set(att(Some(true)), cert(Some(false)))),
      freshReportCard(2, Set(att(Some(true)), cert(Some(true)), bonus(10))),
    )

    val patterns = List(
      ReportCardEvaluationPattern(labwork, Attendance.entryType, 3, BoolBased),
      ReportCardEvaluationPattern(labwork, Certificate.entryType, 3, BoolBased),
    )

    val result = evaluate(cardEntries, patterns)

    result.size shouldBe patterns.size
    result foreach { r =>
      r.label match {
        case Attendance.entryType =>
          r.bool shouldBe true
          r.int shouldBe 3
        case Certificate.entryType =>
          r.bool shouldBe false
          r.int shouldBe 2
        case _ =>
          fail(s"${r.label} was not part of the evaluation patterns")
      }
    }
  }

  "create negative evaluations results if everything is missing" in {
    val student = UUID.randomUUID
    val labwork = UUID.randomUUID
    val freshReportCard = reportCardEntry(student, labwork) _

    val cardEntries = List(
      freshReportCard(0, Set(att(None), cert(None))),
      freshReportCard(1, Set(att(None), cert(None))),
      freshReportCard(2, Set(att(None), cert(None), bonus(0))),
    )

    val patterns = List(
      ReportCardEvaluationPattern(labwork, Attendance.entryType, 3, BoolBased),
      ReportCardEvaluationPattern(labwork, Certificate.entryType, 3, BoolBased),
    )

    val result = evaluate(cardEntries, patterns)

    result.size shouldBe patterns.size
    result foreach { r =>
      r.label match {
        case Attendance.entryType =>
          r.bool shouldBe false
          r.int shouldBe 0
        case Certificate.entryType =>
          r.bool shouldBe false
          r.int shouldBe 0
        case _ =>
          fail(s"${r.label} was not part of the evaluation patterns")
      }
    }
  }

  "do nothing if patterns are empty" in {
    val student = UUID.randomUUID
    val labwork = UUID.randomUUID
    val freshReportCard = reportCardEntry(student, labwork) _

    val cardEntries = List(
      freshReportCard(0, Set(att(Some(true)), cert(Some(true)))),
      freshReportCard(1, Set(att(Some(true)), cert(Some(false)))),
      freshReportCard(2, Set(att(Some(true)), cert(Some(true)), bonus(10))),
    )

    val result = evaluate(cardEntries, List.empty)
    result.isEmpty shouldBe true
  }

  "ignore results if they are not reflected in patterns" in {
    val student = UUID.randomUUID
    val labwork = UUID.randomUUID
    val freshReportCard = reportCardEntry(student, labwork) _

    val cardEntries = List(
      freshReportCard(0, Set(att(Some(true)), cert(Some(true)))),
      freshReportCard(1, Set(att(Some(true)), cert(Some(false)))),
      freshReportCard(2, Set(att(Some(true)), cert(Some(true)), bonus(10))),
    )

    val patterns = List(
      ReportCardEvaluationPattern(labwork, Certificate.entryType, 2, BoolBased),
    )

    val result = evaluate(cardEntries, patterns)

    result.size shouldBe 1
    result.head.label shouldBe Certificate.entryType
    result.head.bool shouldBe true
    result.head.int shouldBe 2
  }

  "don't change fresh evaluations if there are no existing ones" in {
    val student = UUID.randomUUID
    val labwork = UUID.randomUUID
    val freshEval = eval(student, labwork) _

    val newEvals = List(
      freshEval(Attendance.entryType, true, 3),
      freshEval(Certificate.entryType, true, 3),
    )

    val result = deltas(List.empty, newEvals)
    result should contain theSameElementsAs newEvals
  }

  "update an existing evaluation if there is a new delta" in {
    val student = UUID.randomUUID
    val labwork = UUID.randomUUID
    val freshEval = eval(student, labwork) _

    val existingEvals = List(
      freshEval(Attendance.entryType, true, 3),
      freshEval(Certificate.entryType, false, 2),
    )

    val newEvals = List(
      freshEval(Attendance.entryType, true, 3),
      freshEval(Certificate.entryType, true, 3),
    )

    val result = deltas(existingEvals, newEvals)
    result.size shouldBe 1
    result.head.label shouldBe Certificate.entryType
    result.head.bool shouldBe true
    result.head.int shouldBe 3

    val updatedEval = existingEvals.find(_.label == Certificate.entryType).get
    result.head.lastModified shouldBe updatedEval.lastModified
    result.head.id shouldBe updatedEval.id
  }

  "update an existing evaluation and create a new one if there is a delta" in {
    val student = UUID.randomUUID
    val labwork = UUID.randomUUID
    val freshEval = eval(student, labwork) _

    val existingEvals = List(
      freshEval(Attendance.entryType, true, 3),
      freshEval(Certificate.entryType, false, 2),
    )

    val newEvals = List(
      freshEval(Attendance.entryType, true, 3),
      freshEval(Certificate.entryType, true, 3),
      freshEval(Bonus.entryType, true, 10),
    )

    val result = deltas(existingEvals, newEvals)
    result.size shouldBe 2

    val cert = result.find(_.label == Certificate.entryType).get
    cert.bool shouldBe true
    cert.int shouldBe 3

    val updatedEval = existingEvals.find(_.label == Certificate.entryType).get
    cert.lastModified shouldBe updatedEval.lastModified
    cert.id shouldBe updatedEval.id

    val bonus = result.find(_.label == Bonus.entryType).get
    bonus.bool shouldBe true
    bonus.int shouldBe 10
  }

  "skip delta eval if the given student is fast forwarded" in {
    val student = UUID.randomUUID
    val labwork = UUID.randomUUID
    val freshEval = eval(student, labwork) _

    val existingEvals = List(
      ReportCardEvaluation(student, labwork, Attendance.entryType, bool = true, FastForwardValue, DateTime.now),
      ReportCardEvaluation(student, labwork, Certificate.entryType, bool = true, FastForwardValue, DateTime.now),
      ReportCardEvaluation(student, labwork, Bonus.entryType, bool = true, FastForwardValue, DateTime.now),
      ReportCardEvaluation(student, labwork, Supplement.entryType, bool = true, FastForwardValue, DateTime.now),
    )

    val newEvals = List(
      freshEval(Attendance.entryType, true, 3),
      freshEval(Certificate.entryType, true, 3),
    )

    val result = deltas(existingEvals, newEvals)
    result.isEmpty shouldBe true
  }

  "skip delta eval if the given student is fired" in {
    val student = UUID.randomUUID
    val labwork = UUID.randomUUID
    val freshEval = eval(student, labwork) _

    val existingEvals = List(
      ReportCardEvaluation(student, labwork, Attendance.entryType, bool = false, FireValue, DateTime.now),
      ReportCardEvaluation(student, labwork, Certificate.entryType, bool = false, FireValue, DateTime.now),
      ReportCardEvaluation(student, labwork, Bonus.entryType, bool = false, FireValue, DateTime.now),
      ReportCardEvaluation(student, labwork, Supplement.entryType, bool = false, FireValue, DateTime.now),
    )

    val newEvals = List(
      freshEval(Attendance.entryType, true, 3),
      freshEval(Certificate.entryType, true, 3),
    )

    val result = deltas(existingEvals, newEvals)
    result.isEmpty shouldBe true
  }
}
