package services

import java.util.UUID

import base.TestBaseDefinition
import models._
import org.joda.time.{LocalDate, LocalTime}
import org.scalatest.WordSpec

import scala.util.{Failure, Success}

object ReportCardServiceSpec {

  def integer(assEntry: AssignmentEntry, appEntry: ScheduleEntryG, cEntry: ReportCardEntry): Boolean = {
    def integerTypes(left: Set[AssignmentEntryType], right: Set[AssignmentEntryType]): Boolean = {
      import AssignmentEntryType._

      left.map(toProtocol) == right.map(toProtocol)
    }

    assEntry.index == cEntry.index &&
      assEntry.label == cEntry.label &&
      integerTypes(assEntry.types, cEntry.entryTypes) &&
      appEntry.date.isEqual(cEntry.date) &&
      appEntry.start.isEqual(cEntry.start) &&
      appEntry.room == cEntry.room
  }

  def group(students: Int): Group = {
    val members = (0 until students).map(_ => UUID.randomUUID()).toSet
    Group("", UUID.randomUUID(), members)
  }

  def plan(amount: Int): AssignmentPlan = {
    def randomTypes: Set[AssignmentEntryType] = {
      import models.AssignmentEntryType._
      import scala.util.Random._

      val types = all.map(fromProtocol).toVector
      shuffle(types).take(nextInt(types.size)).toSet
    }

    val pe = (0 until amount).map(n => AssignmentEntry(n, n.toString, randomTypes)).toSet
    AssignmentPlan(UUID.randomUUID(), amount, amount, pe)
  }

  def schedule(amount: Int, aps: Int, emptyMembers: Boolean = false): ScheduleG = {
    val se = (0 until amount).map { n =>
      val start = LocalTime.now.plusHours(n)
      val grp = if (emptyMembers) group(0) else group(20)

      ScheduleEntryG(start, start.plusHours(n), LocalDate.now.plusWeeks(n), UUID.randomUUID(), UUID.randomUUID(), grp, UUID.randomUUID())
    }.toVector

    val see = (0 until aps).foldLeft(Vector.empty[ScheduleEntryG]) { (vec, _) =>
      vec ++ se
    }

    ScheduleG(UUID.randomUUID(), see, UUID.randomUUID())
  }
}

class ReportCardServiceSpec extends WordSpec with TestBaseDefinition {

  val reportCardService = new ReportCardService

  "A ReportCardServiceSpec " should {

    "successfully return report cards for given schedule" in {
      import ReportCardServiceSpec._
      import models.schedule.TimetableDateEntry._

      val amount = 8
      val assignmentPlan = plan(amount)
      val scheduleG = schedule(amount, assignmentPlan.entries.size)

      val result = reportCardService.reportCards(scheduleG, assignmentPlan)

      result match {
        case Success(cards) =>
          cards.nonEmpty shouldBe true
          cards.forall(_.entries.size == assignmentPlan.entries.size) shouldBe true
          cards.forall(c => c.entries.size == scheduleG.entries.count(_.group.members.contains(c.student))) shouldBe true

          cards.forall( c =>
            assignmentPlan.entries.flatMap(_.types.map(_.id)).forall(id => c.entries.exists(_.entryTypes.map(_.id).contains(id)))
          ) shouldBe false

          cards.forall { c =>
            val assignments = assignmentPlan.entries.toVector.sortBy(_.index)
            val appointments = scheduleG.entries.filter(_.group.members.contains(c.student)).sortBy(toLocalDateTime)
            val studentApps = c.entries.toVector.sortBy(_.index)

            (assignments, appointments, studentApps).zipped.forall {
              case (ass, app, s) => integer(ass, app, s)
            }
          } shouldBe true
        case Failure(e) =>
          fail("error while creating report cards", e)
      }
    }

    "fail when there are no students" in {
      import ReportCardServiceSpec._

      val amount = 8
      val assignmentPlan = plan(amount)
      val scheduleG = schedule(amount, assignmentPlan.entries.size, emptyMembers = true)
      val expectedError = new Throwable(s"No students found while creating report cards for ${scheduleG.id}")

      val result = reportCardService.reportCards(scheduleG, assignmentPlan)

      result match {
        case Failure(e) =>
          e.getMessage shouldBe expectedError.getMessage
        case _ =>
          fail("report cards should be empty when there are no students")
      }
    }
  }
}
