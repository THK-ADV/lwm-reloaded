package services

/*import java.util.UUID

import base.TestBaseDefinition
import models._
import models.SesameReportCardEntryType._
import org.joda.time.{LocalDate, LocalTime}
import org.scalatest.WordSpec

object ReportCardServiceSpec {

  def integer(assEntry: SesameAssignmentEntry, appEntry: ScheduleEntryG, cEntry: SesameReportCardEntry): Boolean = {
    def integerTypes(left: Set[SesameAssignmentEntryType], right: Set[SesameReportCardEntryType]): Boolean = {
      def toAssignmentEntryType(cardEntry: SesameReportCardEntryType): SesameAssignmentEntryType = {
        SesameAssignmentEntryType(cardEntry.entryType, cardEntry.bool, cardEntry.int)
      }

      left == right.map(toAssignmentEntryType)
    }

    assEntry.label == cEntry.label &&
      integerTypes(assEntry.types, cEntry.entryTypes) &&
      appEntry.date.isEqual(cEntry.date) &&
      appEntry.start.isEqual(cEntry.start) &&
      appEntry.room == cEntry.room
  }

  def group(students: Int): SesameGroup = {
    val members = (0 until students).map(_ => UUID.randomUUID()).toSet
    SesameGroup("", UUID.randomUUID(), members)
  }

  def plan(amount: Int): SesameAssignmentPlan = {
    def randomTypes: Set[SesameAssignmentEntryType] = {
      import scala.util.Random._

      val types = SesameAssignmentEntryType.all.toVector
      shuffle(types).take(nextInt(types.size)).toSet
    }

    val pe = (0 until amount).map(n => SesameAssignmentEntry(n, n.toString, randomTypes)).toSet
    SesameAssignmentPlan(UUID.randomUUID(), amount, amount, pe)
  }

  def schedule(amount: Int, aps: Int): ScheduleG = {
    val initial = (0 until amount).map { n =>
      val start = LocalTime.now.plusHours(n)

      ScheduleEntryG(start, start.plusHours(n), LocalDate.now.plusWeeks(n), UUID.randomUUID(), Set(User.randomUUID), group(20))
    }.toVector

    val see = (0 until aps).foldLeft(Vector.empty[ScheduleEntryG]) { (vec, i) =>
      vec ++ initial.map { o =>
        val deltaStart = o.start.plusHours(i)
        ScheduleEntryG(deltaStart, deltaStart.plusHours(1), o.date.plusWeeks(i), o.room, o.supervisor, o.group)
      }
    }

    ScheduleG(UUID.randomUUID(), see, UUID.randomUUID())
  }
}

class ReportCardServiceSpec extends WordSpec with TestBaseDefinition {

  val reportCardService = new ReportCardService

  val planEntries = {
    import models.SesameAssignmentEntryType._

    Vector(
      SesameAssignmentEntry(0, "Einführung", Set(Attendance)),
      SesameAssignmentEntry(1, "Liveaufgabe 1 - C", Set(Attendance, Certificate)),
      SesameAssignmentEntry(2, "Liveaufgabe 2 - C", Set(Attendance, Certificate)),
      SesameAssignmentEntry(3, "Ilias Test", Set(Attendance, Certificate, Bonus)),
      SesameAssignmentEntry(4, "Liveaufgabe 3 - Java", Set(Attendance, Certificate)),
      SesameAssignmentEntry(5, "Liveaufgabe 4 - Java", Set(Attendance, Certificate)),
      SesameAssignmentEntry(6, "Codereview", Set(Attendance, Certificate, Supplement)),
      SesameAssignmentEntry(7, "Codereview", Set(Attendance, Certificate, Supplement))
    )
  }

  val student = UUID.randomUUID

  "A ReportCardServiceSpec " should {

    "successfully return report cards for given schedule" in {
      import services.ReportCardServiceSpec._
      import models.TimetableDateEntry._
      import models.LwmDateTime.localDateTimeOrd

      val amount = 8
      val assignmentPlan = plan(amount)
      val scheduleG = schedule(amount, assignmentPlan.entries.size)

      val entries = reportCardService.reportCards(scheduleG, assignmentPlan)

      entries.nonEmpty shouldBe true
      entries.size should be(assignmentPlan.entries.size * scheduleG.entries.flatMap(_.group.members).toSet.size)
      entries.groupBy(_.student).forall(m => m._2.size == scheduleG.entries.count(_.group.members.contains(m._1))) shouldBe true
      entries.groupBy(_.student).forall(m =>
        m._2.flatMap(_.entryTypes.map(_.id)).size == assignmentPlan.entries.toVector.flatMap(_.types).size
      ) shouldBe true

      entries.groupBy(_.student).forall { m =>
        val assignments = assignmentPlan.entries.toVector.sortBy(_.index)
        val appointments = scheduleG.entries.filter(_.group.members.contains(m._1)).sortBy(toLocalDateTime)
        val studentApps = m._2.toVector.sortBy(e => e.date.toLocalDateTime(e.start))

        (assignments, appointments, studentApps).zipped.forall {
          case (ass, app, s) => integer(ass, app, s)
        }
      } shouldBe true
    }

    "pass a student's report card when everything is fine" in {
      val bonusPoints = 10
      val cardEntries = planEntries.map { e =>
        val types = e.types.map {
          case att if att.entryType == Attendance.entryType => SesameReportCardEntryType(att.entryType, !(e.index == 0), 0)
          case cert if cert.entryType == Certificate.entryType => SesameReportCardEntryType(cert.entryType, bool = true, 0)
          case bonus if bonus.entryType == Bonus.entryType => SesameReportCardEntryType(bonus.entryType, bool = false, bonusPoints)
          case supp if supp.entryType == Supplement.entryType => SesameReportCardEntryType(supp.entryType, bool = true, 0)
        }

        SesameReportCardEntry(student, UUID.randomUUID, e.label, LocalDate.now, LocalTime.now, LocalTime.now, UUID.randomUUID(), types)
      }
      val types = planEntries.flatMap(_.types)
      val attendance = types.count(_.entryType == Attendance.entryType)
      val mandatory = types.count(_.entryType == Certificate.entryType)
      val assignmentPlan = SesameAssignmentPlan(UUID.randomUUID(), attendance - 1, mandatory, planEntries.toSet)

      val result = reportCardService.evaluate(assignmentPlan, cardEntries.toSet)

      result.size shouldBe SesameReportCardEntryType.all.size
      result.forall(_.bool)
      result.forall(_.int == bonusPoints)
    }

    "pass a student's report card even when he barley performed" in {
      val cardEntries = planEntries.map { e =>
        val types = e.types.map {
          case att if att.entryType == Attendance.entryType => SesameReportCardEntryType(att.entryType, !(e.index == 0 || e.index == 1), 0)
          case cert if cert.entryType == Certificate.entryType => SesameReportCardEntryType(cert.entryType, !(e.index == 1 || e.index == 2 || e.index == 7), 0)
          case bonus if bonus.entryType == Bonus.entryType => SesameReportCardEntryType(bonus.entryType, bool = false, 0)
          case supp if supp.entryType == Supplement.entryType => SesameReportCardEntryType(supp.entryType, bool = true, 0)
        }

        SesameReportCardEntry(student, UUID.randomUUID, e.label, LocalDate.now, LocalTime.now, LocalTime.now, UUID.randomUUID(), types)
      }
      val types = planEntries.flatMap(_.types)
      val attendance = types.count(_.entryType == Attendance.entryType)
      val mandatory = types.count(_.entryType == Certificate.entryType)
      val assignmentPlan = SesameAssignmentPlan(UUID.randomUUID(), attendance - 2, mandatory - 3, planEntries.toSet)

      val result = reportCardService.evaluate(assignmentPlan, cardEntries.toSet)

      result.size shouldBe SesameReportCardEntryType.all.size
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
          case att if att.entryType == Attendance.entryType => SesameReportCardEntryType(att.entryType, bool = true, 0)
          case cert if cert.entryType == Certificate.entryType => SesameReportCardEntryType(cert.entryType, !(e.index == 2 || e.index == 5), 0)
          case bonus if bonus.entryType == Bonus.entryType => SesameReportCardEntryType(bonus.entryType, bool = false, bonusPoints)
          case supp if supp.entryType == Supplement.entryType => SesameReportCardEntryType(supp.entryType, bool = true, 0)
        }

        SesameReportCardEntry(student, UUID.randomUUID, e.label, LocalDate.now, LocalTime.now, LocalTime.now, UUID.randomUUID(), types)
      }
      val types = planEntries.flatMap(_.types.toVector)
      val attendance = types.count(_.entryType == Attendance.entryType)
      val mandatory = types.count(_.entryType == Certificate.entryType)
      val assignmentPlan = SesameAssignmentPlan(UUID.randomUUID(), attendance - 1, mandatory, planEntries.toSet)

      val result = reportCardService.evaluate(assignmentPlan, cardEntries.toSet)

      result.size shouldBe SesameReportCardEntryType.all.size
      result.foreach {
        case att if att.label == Attendance.entryType => att.bool shouldBe true
        case cert if cert.label == Certificate.entryType => cert.bool shouldBe false
        case bonus if bonus.label == Bonus.entryType => bonus.int shouldBe bonusPoints
        case supp if supp.label == Supplement.entryType => supp.bool shouldBe true
      }
    }

    "deny a student's report card when a supplement is missing" in {
      val cardEntries = planEntries.map { e =>
        val types = e.types.map {
          case att if att.entryType == Attendance.entryType => SesameReportCardEntryType(att.entryType, bool = true, 0)
          case cert if cert.entryType == Certificate.entryType => SesameReportCardEntryType(cert.entryType, bool = true, 0)
          case bonus if bonus.entryType == Bonus.entryType => SesameReportCardEntryType(bonus.entryType, bool = false, 0)
          case supp if supp.entryType == Supplement.entryType => SesameReportCardEntryType(supp.entryType, !(e.index == 7), 0)
        }

        SesameReportCardEntry(student, UUID.randomUUID, e.label, LocalDate.now, LocalTime.now, LocalTime.now, UUID.randomUUID(), types)
      }
      val types = planEntries.flatMap(_.types)
      val attendance = types.count(_.entryType == Attendance.entryType)
      val mandatory = types.count(_.entryType == Certificate.entryType)
      val assignmentPlan = SesameAssignmentPlan(UUID.randomUUID(), attendance - 1, mandatory, planEntries.toSet)

      val result = reportCardService.evaluate(assignmentPlan, cardEntries.toSet)

      result.size shouldBe SesameReportCardEntryType.all.size
      result.foreach {
        case att if att.label == Attendance.entryType => att.bool shouldBe true
        case cert if cert.label == Certificate.entryType => cert.bool shouldBe true
        case bonus if bonus.label == Bonus.entryType => bonus.int shouldBe 0
        case supp if supp.label == Supplement.entryType => supp.bool shouldBe false
      }
    }

    "successfully pass a student explicitly" in {
      val student = UUID.randomUUID
      val labwork = UUID.randomUUID
      val result = reportCardService.evaluateExplicit(student, labwork)

      result.forall(eval => eval.student == student && eval.labwork == labwork) shouldBe true
      result.size shouldBe SesameReportCardEntryType.all.size
      result.forall(eval => SesameReportCardEntryType.all.count(_.entryType == eval.label) == 1) shouldBe true
    }
  }
}
*/