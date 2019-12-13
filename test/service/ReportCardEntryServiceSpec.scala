package service

import java.util.UUID

import base.{DateGenerator, TestBaseDefinition}
import models.genesis.ScheduleEntryGen
import models.{assignment, _}
import models.assignment.{AssignmentEntry, AssignmentType}
import org.joda.time.LocalDate
import org.scalatest.WordSpec

final class ReportCardEntryServiceSpec extends WordSpec with TestBaseDefinition with DateGenerator {

  import service.ReportCardEntryService._

  "A ReportCardEntryServiceSpec" should {

    "not generate report card entries if schedule and/or assignment entries are empty" in {
      val entries = generateReportCardEntries(List.empty, List.empty)

      entries.failure.exception.getMessage.contains("can't generate") shouldBe true
    }

    "not generate report card entries if schedule entries can't cover assignment entries" in {
      val scheduleEntries = List(scheduleEntry(9, 10, 2019, 1, 1, Group("", UUID.randomUUID(), uuids(2))))
      val assignments = assignmentEntries(3)
      val entries = generateReportCardEntries(toAtom(scheduleEntries, UUID.randomUUID), assignments)

      entries.failure.exception.getMessage.contains("can't distribute") shouldBe true
    }

    "generate report card entries using a schedule and assignment entries" in {
      val labwork = UUID.randomUUID
      val grpA = Group("A", labwork, uuids(5))
      val grpB = Group("B", labwork, uuids(5))
      val grpC = Group("C", labwork, uuids(3))
      val grpD = Group("D", labwork, uuids(3))
      val grpE = Group("E", labwork, uuids(2))
      val grpF = Group("F", labwork, uuids(4))

      val scheduleEntries = List(
        scheduleEntry(9, 10, 2019, 1, 1, grpA),
        scheduleEntry(10, 11, 2019, 1, 1, grpB),
        scheduleEntry(11, 12, 2019, 1, 1, grpC),
        scheduleEntry(9, 10, 2019, 1, 2, grpD),
        scheduleEntry(10, 11, 2019, 1, 2, grpE),
        scheduleEntry(9, 11, 2019, 1, 3, grpF),
        scheduleEntry(9, 10, 2019, 2, 1, grpA),
        scheduleEntry(10, 11, 2019, 2, 1, grpB),
        scheduleEntry(11, 12, 2019, 2, 1, grpC),
        scheduleEntry(9, 10, 2019, 2, 2, grpD),
        scheduleEntry(10, 11, 2019, 2, 2, grpE),
        scheduleEntry(9, 11, 2019, 2, 3, grpF),
        scheduleEntry(8, 10, 2019, 3, 1, grpA),
        scheduleEntry(10, 11, 2019, 3, 1, grpB),
        scheduleEntry(11, 12, 2019, 3, 1, grpC),
        scheduleEntry(9, 10, 2019, 3, 2, grpD),
        scheduleEntry(10, 11, 2019, 3, 2, grpE),
        scheduleEntry(9, 11, 2019, 3, 3, grpF)
      )

      val assignments = assignmentEntries(3)
      val reportCardEntries = generateReportCardEntries(toAtom(scheduleEntries, labwork), assignments).success.value
      val students = 5 + 5 + 3 + 3 + 2 + 4

      reportCardEntries.size shouldBe students * assignments.size
      reportCardEntries.groupBy(_.student).foreach(_._2.size == assignments.size shouldBe true)

      val appointmentsGrpA = reportCardEntries
        .filter(_.student == grpA.members.head)
        .map(e => (e.assignmentIndex, e.date, e.start, e.end))

      appointmentsGrpA shouldBe List(
        (0, localDate(2019, 1, 1), localTime(9), localTime(10)),
        (1, localDate(2019, 2, 1), localTime(9), localTime(10)),
        (2, localDate(2019, 3, 1), localTime(8), localTime(10))
      )

      val appointmentsGrpD = reportCardEntries
        .filter(_.student == grpD.members.head)
        .map(e => (e.assignmentIndex, e.date, e.start, e.end))

      appointmentsGrpD shouldBe List(
        (0, localDate(2019, 1, 2), localTime(9), localTime(10)),
        (1, localDate(2019, 2, 2), localTime(9), localTime(10)),
        (2, localDate(2019, 3, 2), localTime(9), localTime(10))
      )
    }
  }

  private def scheduleEntry(start: Int, end: Int, year: Int, month: Int, day: Int, grp: Group): ScheduleEntryGen = ScheduleEntryGen(
    localTime(start), localTime(end), localDate(year, month, day), UUID.randomUUID, Set.empty, grp
  )

  private def toAtom(xs: List[ScheduleEntryGen], labwork: UUID): List[ScheduleEntryAtom] = {
    val semester = Semester("", "", LocalDate.now, LocalDate.now, LocalDate.now, UUID.randomUUID)
    val user = Employee("", "", "", "", UUID.randomUUID)
    val course = CourseAtom("", "", "", user, 1, UUID.randomUUID)
    val degree = Degree("", "", UUID.randomUUID)
    val atom = LabworkAtom("", "", semester, course, degree, subscribable = false, published = false, UUID.randomUUID)

    xs.map(x => ScheduleEntryAtom(atom, x.start, x.end, x.date, Room("", "", 1, x.room), Set.empty, x.group, UUID.randomUUID))
  }

  private def uuids(n: Int) = (0 until n).map(_ => UUID.randomUUID).toSet

  private def assignmentEntries(n: Int): List[AssignmentEntry] = {
    val labwork = UUID.randomUUID

    (0 until n)
      .map(i => assignment.AssignmentEntry(labwork, i, i.toString, assignmentEntryTypes(), 1, UUID.randomUUID))
      .toList
  }

  private def assignmentEntryTypes(): Set[AssignmentType] = {
    import scala.util.Random.nextInt
    ??? // TODO
    //AssignmentEntryType.all.take(1 + nextInt(AssignmentEntryType.all.size - 1))
  }
}
