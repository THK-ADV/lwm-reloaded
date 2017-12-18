package services

import java.util.UUID

import base.TestBaseDefinition
import models._
import org.joda.time.{DateTime, LocalDate, LocalTime}
import org.scalatest.WordSpec
import services.ReportCardService.{BoolBased, IntBased, ReportCardEvaluationPattern}

final class ReportCardService2Spec extends WordSpec with TestBaseDefinition {

  import utils.LwmDateTime._
  import models.PostgresReportCardEntryType._

  "A ReportCardService2Spec" should {

    "successfully return report cards for given schedule" in {
      val amount = 8
      val assignmentPlan = plan(amount)
      val scheduleG = schedule(amount, assignmentPlan.entries.size)

      val entries = ReportCardService.reportCards(scheduleG, assignmentPlan)

      entries.nonEmpty shouldBe true
      entries.size should be(assignmentPlan.entries.size * scheduleG.entries.flatMap(_.group.members).toSet.size)
      entries.groupBy(_.student).forall(m => m._2.size == scheduleG.entries.count(_.group.members.contains(m._1))) shouldBe true
      entries.groupBy(_.student).forall(m =>
        m._2.flatMap(_.entryTypes.map(_.id)).size == assignmentPlan.entries.toVector.flatMap(_.types).size
      ) shouldBe true

      entries.groupBy(_.student).forall { m =>
        val assignments = assignmentPlan.entries.toVector.sortBy(_.index)
        val appointments = scheduleG.entries.filter(_.group.members.contains(m._1)).sortBy(toLocalDateTime)
        val studentApps = m._2.sortBy(e => e.date.localDate.toLocalDateTime(e.start.localTime))

        (assignments, appointments, studentApps).zipped.forall {
          case (ass, app, s) => integer(ass, app, s.toLwmModel)
        }
      } shouldBe true
    }

    "pass a student's report card when everything is fine" in {
      val bonusPoints = 10
      val cardEntries = planEntries.map { e =>
        val types = e.types.map {
          case att if att.entryType == Attendance.entryType => PostgresReportCardEntryType(att.entryType, Some(!(e.index == 0)))
          case cert if cert.entryType == Certificate.entryType => PostgresReportCardEntryType(cert.entryType, Some(true))
          case bonus if bonus.entryType == Bonus.entryType => PostgresReportCardEntryType(bonus.entryType, Some(false), bonusPoints)
          case supp if supp.entryType == Supplement.entryType => PostgresReportCardEntryType(supp.entryType, Some(true))
        }

        PostgresReportCardEntry(student, UUID.randomUUID, e.label, LocalDate.now, LocalTime.now, LocalTime.now, UUID.randomUUID(), types)
      }
      val types = planEntries.flatMap(_.types)
      val attendance = types.count(_.entryType == Attendance.entryType)
      val mandatory = types.count(_.entryType == Certificate.entryType)
      val pattern = List(
        ReportCardEvaluationPattern(Attendance.entryType, attendance - 1, BoolBased),
        ReportCardEvaluationPattern(Certificate.entryType, mandatory, BoolBased),
        ReportCardEvaluationPattern(Bonus.entryType, 0, IntBased),
        ReportCardEvaluationPattern(Supplement.entryType, 0, BoolBased)
      )

      val result = ReportCardService.evaluate(cardEntries.toList, pattern)

      result.size shouldBe PostgresReportCardEntryType.all.size
      result.forall(_.bool) shouldBe true
      result.find(r => r.label == Bonus.entryType && r.int == bonusPoints) shouldBe defined
    }

    "pass a student's report card even when he barley performed" in {
      val cardEntries = planEntries.map { e =>
        val types = e.types.map {
          case att if att.entryType == Attendance.entryType => PostgresReportCardEntryType(att.entryType, Some(!(e.index == 0 || e.index == 1)))
          case cert if cert.entryType == Certificate.entryType => PostgresReportCardEntryType(cert.entryType, Some(!(e.index == 1 || e.index == 2 || e.index == 7)))
          case bonus if bonus.entryType == Bonus.entryType => PostgresReportCardEntryType(bonus.entryType, Some(false))
          case supp if supp.entryType == Supplement.entryType => PostgresReportCardEntryType(supp.entryType, Some(true))
        }

        PostgresReportCardEntry(student, UUID.randomUUID, e.label, LocalDate.now, LocalTime.now, LocalTime.now, UUID.randomUUID(), types)
      }
      val types = planEntries.flatMap(_.types)
      val attendance = types.count(_.entryType == Attendance.entryType)
      val mandatory = types.count(_.entryType == Certificate.entryType)
      val pattern = List(
        ReportCardEvaluationPattern(Attendance.entryType, attendance - 2, BoolBased),
        ReportCardEvaluationPattern(Certificate.entryType, mandatory - 3, BoolBased),
        ReportCardEvaluationPattern(Bonus.entryType, 0, IntBased),
        ReportCardEvaluationPattern(Supplement.entryType, 0, BoolBased)
      )

      val result = ReportCardService.evaluate(cardEntries.toList, pattern)

      result.size shouldBe PostgresReportCardEntryType.all.size
      result.foreach {
        case att if att.label == Attendance.entryType => att.bool shouldBe true
        case cert if cert.label == Certificate.entryType => cert.bool shouldBe true
        case bonus if bonus.label == Bonus.entryType => bonus.int shouldBe 0
        case supp if supp.label == Supplement.entryType => supp.bool shouldBe true
      }
    }

    "deny a student's report card when some mandatory certificates are missing" in {
      val bonusPoints = 5
      val cardEntries = planEntries.map { e =>
        val types = e.types.map {
          case att if att.entryType == Attendance.entryType => PostgresReportCardEntryType(att.entryType, Some(true))
          case cert if cert.entryType == Certificate.entryType => PostgresReportCardEntryType(cert.entryType, Some(!(e.index == 2 || e.index == 5)))
          case bonus if bonus.entryType == Bonus.entryType => PostgresReportCardEntryType(bonus.entryType, Some(false), bonusPoints)
          case supp if supp.entryType == Supplement.entryType => PostgresReportCardEntryType(supp.entryType, Some(true))
        }

        PostgresReportCardEntry(student, UUID.randomUUID, e.label, LocalDate.now, LocalTime.now, LocalTime.now, UUID.randomUUID(), types)
      }
      val types = planEntries.flatMap(_.types.toVector)
      val attendance = types.count(_.entryType == Attendance.entryType)
      val mandatory = types.count(_.entryType == Certificate.entryType)
      val pattern = List(
        ReportCardEvaluationPattern(Attendance.entryType, attendance - 1, BoolBased),
        ReportCardEvaluationPattern(Certificate.entryType, mandatory, BoolBased),
        ReportCardEvaluationPattern(Bonus.entryType, 0, IntBased),
        ReportCardEvaluationPattern(Supplement.entryType, 0, BoolBased)
      )

      val result = ReportCardService.evaluate(cardEntries.toList, pattern)

      result.size shouldBe PostgresReportCardEntryType.all.size
      result.foreach {
        case att if att.label == Attendance.entryType => att.bool shouldBe true
        case cert if cert.label == Certificate.entryType => cert.bool shouldBe false
        case bonus if bonus.label == Bonus.entryType => bonus.int shouldBe bonusPoints
        case supp if supp.label == Supplement.entryType => supp.bool shouldBe true
      }
    }

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

    "return only those evaluation which matches given pattern" in {
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
        ReportCardEvaluationPattern(Attendance.entryType, 2, BoolBased)
      )

      val result = ReportCardService.evaluate(cards, futurePattern)

      result.size shouldBe 1
      result.head.label shouldBe Attendance.entryType
      result.head.bool shouldBe true
      result.head.int shouldBe 2
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

    "evaluate a student explicit" in {
      val result = ReportCardService.evaluateExplicit(UUID.randomUUID, UUID.randomUUID)

      result.size shouldBe 4
      result.foreach { eval =>
        PostgresReportCardEntryType.all.count(_.entryType == eval.label) shouldBe 1
        eval.bool shouldBe true
        eval.int shouldBe ReportCardService.EvaluatedExplicit
      }
    }

    "skip evaluation if a given student was evaluated explicit" in {
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

      val explicit = ReportCardService.evaluateExplicit(student, labwork)
      val result = ReportCardService.evaluateDeltas(cards, futurePattern, explicit.map(_.toLwmModel))

      result shouldBe empty
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

  val student: UUID = UUID.randomUUID
  val labwork: UUID = UUID.randomUUID

  val template: (Set[PostgresReportCardEntryType]) => PostgresReportCardEntry = partialReportCardEntry(student, labwork)

  private def partialReportCardEntry(student: UUID, labwork: UUID)(types: Set[PostgresReportCardEntryType]) = {
    PostgresReportCardEntry(student, labwork, "", LocalDate.now, LocalTime.now, LocalTime.now, UUID.randomUUID, types)
  }

  private def partialReportCardEvaluation(student: UUID, labwork: UUID)(label: String, bool: Boolean, int: Int) = {
    PostgresReportCardEvaluation(student, labwork, label, bool, int, DateTime.now)
  }

  private def partialReportCardEvaluationDb(student: UUID, labwork: UUID)(label: String, bool: Boolean, int: Int) = {
    ReportCardEvaluationDb(student, labwork, label, bool, int)
  }

  private def plan(amount: Int) = {
    def randomTypes: Set[PostgresAssignmentEntryType] = {
      import scala.util.Random._

      val types = PostgresAssignmentEntryType.all.toVector
      shuffle(types).take(nextInt(types.size)).toSet
    }

    val pe = (0 until amount).map(n => PostgresAssignmentEntry(n, n.toString, randomTypes)).toSet
    PostgresAssignmentPlan(UUID.randomUUID, amount, amount, pe)
  }

  private def group(students: Int): PostgresGroup = {
    PostgresGroup("", UUID.randomUUID, (0 until students).map(_ => UUID.randomUUID).toSet)
  }

  private def schedule(amount: Int, aps: Int): ScheduleGen = {
    val initial = (0 until amount).map { n =>
      val start = LocalTime.now.plusHours(n)

      ScheduleEntryGen(start, start.plusHours(n), LocalDate.now.plusWeeks(n), UUID.randomUUID, Set(User.randomUUID), group(20))
    }.toVector

    val see = (0 until aps).foldLeft(Vector.empty[ScheduleEntryGen]) { (vec, i) =>
      vec ++ initial.map { o =>
        val deltaStart = o.start.plusHours(i)
        ScheduleEntryGen(deltaStart, deltaStart.plusHours(1), o.date.plusWeeks(i), o.room, o.supervisor, o.group)
      }
    }

    ScheduleGen(UUID.randomUUID, see)
  }

  def integer(assEntry: PostgresAssignmentEntry, appEntry: ScheduleEntryGen, cEntry: PostgresReportCardEntry): Boolean = {
    def integerTypes(left: Set[PostgresAssignmentEntryType], right: Set[PostgresReportCardEntryType]): Boolean = {
      def toAssignmentEntryType(cardEntry: PostgresReportCardEntryType): PostgresAssignmentEntryType = {
        PostgresAssignmentEntryType(cardEntry.entryType, cardEntry.bool.getOrElse(false), cardEntry.int)
      }

      left == right.map(toAssignmentEntryType)
    }

    assEntry.label == cEntry.label &&
      integerTypes(assEntry.types, cEntry.entryTypes) &&
      appEntry.date.isEqual(cEntry.date) &&
      appEntry.start.isEqual(cEntry.start) &&
      appEntry.room == cEntry.room
  }

  val planEntries: Vector[PostgresAssignmentEntry] = {
    import models.PostgresAssignmentEntryType._

    Vector(
      PostgresAssignmentEntry(0, "Einführung", Set(Attendance)),
      PostgresAssignmentEntry(1, "Liveaufgabe 1 - C", Set(Attendance, Certificate)),
      PostgresAssignmentEntry(2, "Liveaufgabe 2 - C", Set(Attendance, Certificate)),
      PostgresAssignmentEntry(3, "Ilias Test", Set(Attendance, Certificate, Bonus)),
      PostgresAssignmentEntry(4, "Liveaufgabe 3 - Java", Set(Attendance, Certificate)),
      PostgresAssignmentEntry(5, "Liveaufgabe 4 - Java", Set(Attendance, Certificate)),
      PostgresAssignmentEntry(6, "Codereview", Set(Attendance, Certificate, Supplement)),
      PostgresAssignmentEntry(7, "Codereview", Set(Attendance, Certificate, Supplement))
    )
  }
}
