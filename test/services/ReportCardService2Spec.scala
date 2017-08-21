package services

import java.util.UUID

import base.TestBaseDefinition
import models.{PostgresReportCardEntry, PostgresReportCardEntryType, PostgresReportCardEvaluation, ReportCardEvaluationDb}
import org.joda.time.{LocalDate, LocalTime}
import org.scalatest.WordSpec
import services.ReportCardService.{BoolBased, IntBased, ReportCardEvaluationPattern}

final class ReportCardService2Spec extends WordSpec with TestBaseDefinition {

  import models.PostgresReportCardEntryType._

  val template: (Set[PostgresReportCardEntryType]) => PostgresReportCardEntry = partialReportCardEntry(UUID.randomUUID, UUID.randomUUID)

  "A ReportCardService2Spec" should {
    "evaluate reportCardEntries against different patterns" in {
      val cards = List(
        template(Set(
          PostgresReportCardEntryType(Attendance.entryType, Some(true)),
          PostgresReportCardEntryType(Certificate.entryType, Some(true))
        )),
        template(Set(
          PostgresReportCardEntryType(Attendance.entryType, Some(false)),
          PostgresReportCardEntryType(Certificate.entryType, None)
        )),
        template(Set(
          PostgresReportCardEntryType(Attendance.entryType, Some(true)),
          PostgresReportCardEntryType(Certificate.entryType, Some(true))
        )),
        template(Set(
          PostgresReportCardEntryType(Attendance.entryType, Some(true)),
          PostgresReportCardEntryType(Certificate.entryType, Some(false))
        )),
        template(Set(
          PostgresReportCardEntryType(Attendance.entryType, Some(true)),
          PostgresReportCardEntryType(Certificate.entryType, Some(true)),
          PostgresReportCardEntryType(Bonus.entryType, int = 10)
        ))
      )

      val passPattern = List(
        ReportCardEvaluationPattern(Attendance.entryType, 3, BoolBased),
        ReportCardEvaluationPattern(Certificate.entryType, 3, BoolBased),
        ReportCardEvaluationPattern(Bonus.entryType, 10, IntBased),
        ReportCardEvaluationPattern(Supplement.entryType, 0, BoolBased)
      )

      val failPattern = List(
        ReportCardEvaluationPattern(Attendance.entryType, 4, BoolBased),
        ReportCardEvaluationPattern(Certificate.entryType, 4, BoolBased),
        ReportCardEvaluationPattern(Bonus.entryType, 15, IntBased),
        ReportCardEvaluationPattern(Supplement.entryType, 0, BoolBased)
      )

      val zeroPattern = List(
        ReportCardEvaluationPattern(Attendance.entryType, 0, BoolBased),
        ReportCardEvaluationPattern(Certificate.entryType, 0, BoolBased),
        ReportCardEvaluationPattern(Bonus.entryType, 0, IntBased),
        ReportCardEvaluationPattern(Supplement.entryType, 0, BoolBased)
      )

      val result = ReportCardService.evaluate(cards, passPattern)
      val result2 = ReportCardService.evaluate(cards, failPattern)
      val result3 = ReportCardService.evaluate(cards, zeroPattern)

      assert(result, passPattern, attBool = true, 4, certBool = true, 3, bonBool = true, 10, suppBool = true, 0)
      assert(result2, failPattern, attBool = true, 4, certBool = false, 3, bonBool = false, 10, suppBool = true, 0)
      assert(result3, zeroPattern, attBool = true, 4, certBool = true, 3, bonBool = true, 10, suppBool = true, 0)
    }

    "return negative evaluation when reportCardEntries are empty" in {
      val cards = List(
        template(Set(
          PostgresReportCardEntryType(Attendance.entryType),
          PostgresReportCardEntryType(Certificate.entryType)
        )),
        template(Set(
          PostgresReportCardEntryType(Attendance.entryType),
          PostgresReportCardEntryType(Certificate.entryType)
        )),
        template(Set(
          PostgresReportCardEntryType(Attendance.entryType),
          PostgresReportCardEntryType(Certificate.entryType)
        )),
        template(Set(
          PostgresReportCardEntryType(Attendance.entryType),
          PostgresReportCardEntryType(Certificate.entryType)
        )),
        template(Set(
          PostgresReportCardEntryType(Attendance.entryType),
          PostgresReportCardEntryType(Certificate.entryType),
          PostgresReportCardEntryType(Bonus.entryType)
        ))
      )

      val futurePattern = List(
        ReportCardEvaluationPattern(Attendance.entryType, 3, BoolBased),
        ReportCardEvaluationPattern(Certificate.entryType, 3, BoolBased),
        ReportCardEvaluationPattern(Bonus.entryType, 10, IntBased),
        ReportCardEvaluationPattern(Supplement.entryType, 0, BoolBased)
      )

      val result = ReportCardService.evaluate(cards, futurePattern)

      assert(result, futurePattern, attBool = false, 0, certBool = false, 0, bonBool = false, 0, suppBool = true, 0)
    }

    "return a pending evaluation when reportCardEntries are half set" in {
      val cards = List(
        template(Set(
          PostgresReportCardEntryType(Attendance.entryType, Some(true)),
          PostgresReportCardEntryType(Certificate.entryType, Some(true))
        )),
        template(Set(
          PostgresReportCardEntryType(Attendance.entryType, Some(true)),
          PostgresReportCardEntryType(Certificate.entryType, Some(true))
        )),
        template(Set(
          PostgresReportCardEntryType(Attendance.entryType),
          PostgresReportCardEntryType(Certificate.entryType)
        )),
        template(Set(
          PostgresReportCardEntryType(Attendance.entryType),
          PostgresReportCardEntryType(Certificate.entryType)
        )),
        template(Set(
          PostgresReportCardEntryType(Attendance.entryType),
          PostgresReportCardEntryType(Certificate.entryType),
          PostgresReportCardEntryType(Bonus.entryType)
        ))
      )

      val futurePattern = List(
        ReportCardEvaluationPattern(Attendance.entryType, 3, BoolBased),
        ReportCardEvaluationPattern(Certificate.entryType, 3, BoolBased),
        ReportCardEvaluationPattern(Bonus.entryType, 10, IntBased),
        ReportCardEvaluationPattern(Supplement.entryType, 0, BoolBased)
      )

      val result = ReportCardService.evaluate(cards, futurePattern)

      assert(result, futurePattern, attBool = false, 2, certBool = false, 2, bonBool = false, 0, suppBool = true, 0)
    }

    "evaluate deltas of given student" in {
      val partialEval = partialReportCardEvaluation(UUID.randomUUID, UUID.randomUUID) _
      val partialEvalDb = partialReportCardEvaluationDb(UUID.randomUUID, UUID.randomUUID) _

      val oldEvals = List(
        partialEval(Attendance.entryType, false, 0), // delta
        partialEval(Certificate.entryType, false, 0), // delta
        partialEval(Bonus.entryType, false, 0), // delta
        partialEval(Supplement.entryType, false, 0) // delta
      )

      val oldEvals2 = List(
        partialEval(Attendance.entryType, true, 5),
        partialEval(Certificate.entryType, false, 0), // delta
        partialEval(Bonus.entryType, true, 10),
        partialEval(Supplement.entryType, false, 0) // delta
      )

      val oldEvals3 = List(
        partialEval(Attendance.entryType, true, 5),
        partialEval(Certificate.entryType, false, 2),
        partialEval(Bonus.entryType, true, 10),
        partialEval(Supplement.entryType, true, 0)
      )

      val newEvals = List(
        partialEvalDb(Attendance.entryType, true, 5),
        partialEvalDb(Certificate.entryType, false, 2),
        partialEvalDb(Bonus.entryType, true, 10),
        partialEvalDb(Supplement.entryType, true, 0)
      )

      val result = ReportCardService.deltas(oldEvals, newEvals)
      result.size shouldBe 4
      result shouldBe List(
        newEvals(0).copy(id = oldEvals(0).id),
        newEvals(1).copy(id = oldEvals(1).id),
        newEvals(2).copy(id = oldEvals(2).id),
        newEvals(3).copy(id = oldEvals(3).id)
      )

      val result2 = ReportCardService.deltas(oldEvals2, newEvals)
      result2.size shouldBe 2
      result2 shouldBe List(
        newEvals(1).copy(id = oldEvals2(1).id),
        newEvals(3).copy(id = oldEvals2(3).id)
      )

      val result3 = ReportCardService.deltas(oldEvals3, newEvals)
      result3 shouldBe empty
    }

    "evaluate deltas even when they are not even" in {
      val partialEval = partialReportCardEvaluation(UUID.randomUUID, UUID.randomUUID) _
      val partialEvalDb = partialReportCardEvaluationDb(UUID.randomUUID, UUID.randomUUID) _

      val oldEvals = List(
        partialEval(Attendance.entryType, true, 5),
        partialEval(Certificate.entryType, false, 0), // delta
        partialEval(Bonus.entryType, false, 0), // delta
        partialEval(Supplement.entryType, false, 0), // delta
        partialEval("more stuff", true, 0),
        partialEval("even more", false, 0)
      )

      val newEvals = List(
        partialEvalDb(Attendance.entryType, true, 5),
        partialEvalDb(Certificate.entryType, false, 2),
        partialEvalDb(Bonus.entryType, true, 10),
        partialEvalDb(Supplement.entryType, true, 0)
      )

      val oldEvals2 = List(
        partialEval(Attendance.entryType, true, 5),
        partialEval(Certificate.entryType, false, 0), // delta
        partialEval(Bonus.entryType, false, 0), // delta
        partialEval(Supplement.entryType, false, 0) // delta
      )

      val newEvals2 = List(
        partialEvalDb(Attendance.entryType, true, 5),
        partialEvalDb(Certificate.entryType, false, 2),
        partialEvalDb(Bonus.entryType, true, 10),
        partialEvalDb(Supplement.entryType, true, 0),
        partialEvalDb("lol", true, 2),
        partialEvalDb("yet another lol", false, 0)
      )

      val result = ReportCardService.deltas(oldEvals, newEvals)
      result.size shouldBe 3
      result shouldBe List(
        newEvals(1).copy(id = oldEvals(1).id),
        newEvals(2).copy(id = oldEvals(2).id),
        newEvals(3).copy(id = oldEvals(3).id)
      )

      val result2 = ReportCardService.deltas(oldEvals2, newEvals2)
      result2.size shouldBe 5
      result2 shouldBe List(
        newEvals2(1).copy(id = oldEvals2(1).id),
        newEvals2(2).copy(id = oldEvals2(2).id),
        newEvals2(3).copy(id = oldEvals2(3).id),
        newEvals2(4),
        newEvals2(5)
      )
    }
  }

  private def assert(result: List[ReportCardEvaluationDb], patterns: List[ReportCardEvaluationPattern], attBool: Boolean, attInt: Int, certBool: Boolean, certInt: Int, bonBool: Boolean, bonInt: Int, suppBool: Boolean, suppInt: Int) {
    result.size shouldBe patterns.size
    result.map(_.label) shouldBe patterns.map(_.entryType)

    result.foreach {
      case att if att.label == Attendance.entryType =>
        att.bool shouldBe attBool
        att.int shouldBe attInt
      case cert if cert.label == Certificate.entryType =>
        cert.bool shouldBe certBool
        cert.int shouldBe certInt
      case bon if bon.label == Bonus.entryType =>
        bon.bool shouldBe bonBool
        bon.int shouldBe bonInt
      case supp if supp.label == Supplement.entryType =>
        supp.bool shouldBe suppBool
        supp.int shouldBe suppInt
    }
  }

  private def partialReportCardEntry(student: UUID, labwork: UUID)(types: Set[PostgresReportCardEntryType]) = {
    PostgresReportCardEntry(student, labwork, "", LocalDate.now, LocalTime.now, LocalTime.now, UUID.randomUUID, types)
  }

  private def partialReportCardEvaluation(student: UUID, labwork: UUID)(label: String, bool: Boolean, int: Int) = {
    PostgresReportCardEvaluation(student, labwork, label, bool, int)
  }

  private def partialReportCardEvaluationDb(student: UUID, labwork: UUID)(label: String, bool: Boolean, int: Int) = {
    ReportCardEvaluationDb(student, labwork, label, bool, int)
  }
}
