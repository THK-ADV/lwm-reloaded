package service

import java.util.UUID

import base.{DateGenerator, TestBaseDefinition}
import models.genesis.ScheduleEntryGen
import models._
import org.joda.time.{LocalDate, LocalTime}
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

    "not extend reportCardEntries if there are no cards" in {
      val descriptions = List(ReportCardEntryDescription("", LocalDate.now, LocalTime.now, LocalTime.now, UUID.randomUUID, Set("1")))
      extendReportCardEntries(descriptions, Nil) shouldBe empty
    }

    "not extend reportCardEntries if there is nothing to extend" in {
      val cards = List(
        ReportCardEntry(UUID.randomUUID, UUID.randomUUID, "01", LocalDate.now, LocalTime.now, LocalTime.now, UUID.randomUUID, Set(ReportCardEntryType("01")), 0)
      )

      extendReportCardEntries(Nil, cards) shouldBe cards
    }

    "extend reportCardEntries by increasing assignmentIndex" in {
      def isSame(x: ReportCardEntry, index: Int, label: String) =
        x.label == label && x.assignmentIndex == index

      val student = UUID.randomUUID
      val labwork = UUID.randomUUID

      val cards = List(
        ReportCardEntry(student, labwork, "01", LocalDate.now, LocalTime.now, LocalTime.now, UUID.randomUUID, Set(ReportCardEntryType("01")), 0),
        ReportCardEntry(student, labwork, "02", LocalDate.now, LocalTime.now, LocalTime.now, UUID.randomUUID, Set(ReportCardEntryType("02")), 1),
      )

      val descs = List(
        ReportCardEntryDescription("1", LocalDate.now, LocalTime.now, LocalTime.now, UUID.randomUUID, Set("1")),
        ReportCardEntryDescription("2", LocalDate.now, LocalTime.now, LocalTime.now, UUID.randomUUID, Set("2")),
        ReportCardEntryDescription("3", LocalDate.now, LocalTime.now, LocalTime.now, UUID.randomUUID, Set("3")),
      )

      val res = extendReportCardEntries(descs, cards)

      res.size shouldBe 5
      res.forall(_.student == student) shouldBe true
      res.forall(_.labwork == labwork) shouldBe true

      val sorted = res.sortBy(_.assignmentIndex)
      isSame(sorted(0), 0, "01") shouldBe true
      isSame(sorted(1), 1, "02") shouldBe true
      isSame(sorted(2), 2, "1") shouldBe true
      isSame(sorted(3), 3, "2") shouldBe true
      isSame(sorted(4), 4, "3") shouldBe true
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
      .map(i => AssignmentEntry(labwork, i, i.toString, assignmentEntryTypes(), 1, UUID.randomUUID))
      .toList
  }

  private def assignmentEntryTypes(): Set[AssignmentEntryType] = {
    import scala.util.Random.nextInt
    AssignmentEntryType.all.take(1 + nextInt(AssignmentEntryType.all.size - 1))
  }
}
