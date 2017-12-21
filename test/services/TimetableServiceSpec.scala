package services

import java.util.UUID

import base.TestBaseDefinition
import utils.LwmDateTime._
import models._
import org.joda.time._
import org.joda.time.format.DateTimeFormat
import org.scalatest.WordSpec

import scala.util.Random.nextInt

class TimetableServiceSpec extends WordSpec with TestBaseDefinition {

  "A TimetableService" should {

    "extrapolate further entries based on frontend's timetable protocol template and assignment plan where some assignments takes more than one week with blacklists applied" in {
      val tt = timetable
      val aEntries = (0 until 7).map {
        case e if e < 5 => PostgresAssignmentEntry(e, "label", Set.empty[PostgresAssignmentEntryType])
        case e => PostgresAssignmentEntry(e, "label", Set.empty[PostgresAssignmentEntryType], e - 3)
      }.toSet
      val plan = PostgresAssignmentPlan(tt.labwork, aEntries.size, aEntries.size, aEntries)
      val groupSize = 6

      val expectedStart = Vector(
        fdt.parseLocalDateTime("19/10/2015 11:00:00"),
        fdt.parseLocalDateTime("26/10/2015 13:00:00"),
        fdt.parseLocalDateTime("02/11/2015 15:00:00"),
        fdt.parseLocalDateTime("09/11/2015 17:00:00"),
        fdt.parseLocalDateTime("20/11/2015 15:00:00"),
        fdt.parseLocalDateTime("14/12/2015 13:00:00"),
        fdt.parseLocalDateTime("29/01/2016 15:00:00")
      )

      val result = TimetableService.extrapolateTimetableByWeeks(tt, weeks, globalBlacklist2, plan, groupSize)

      checkAssertion(tt, plan, groupSize, expectedStart, result)
    }

    "extrapolate further entries based on frontend's timetable protocol template and assignment plan where each assignment takes 2 weeks with blacklists applied" in {
      val tt = timetable
      val aEntries = (0 until 5).map(PostgresAssignmentEntry(_, "label", Set.empty[PostgresAssignmentEntryType], 2)).toSet
      val plan = PostgresAssignmentPlan(tt.labwork, aEntries.size, aEntries.size, aEntries)
      val groupSize = 6

      val expectedStart = Vector(
        fdt.parseLocalDateTime("19/10/2015 11:00:00"),
        fdt.parseLocalDateTime("02/11/2015 15:00:00"),
        fdt.parseLocalDateTime("20/11/2015 15:00:00"),
        fdt.parseLocalDateTime("14/12/2015 13:00:00"),
        fdt.parseLocalDateTime("18/01/2016 17:00:00")
      )

      val result = TimetableService.extrapolateTimetableByWeeks(tt, weeks, globalBlacklist2, plan, groupSize)

      checkAssertion(tt, plan, groupSize, expectedStart, result)
    }

    "extrapolate further entries based on frontend's timetable protocol template and assignment plan where some assignments takes more than one week with local and global blacklists applied" in {
      val localBlacklist = Vector(
        fdt.parseLocalDateTime("30/10/2015 15:00:00"),
        fdt.parseLocalDateTime("06/11/2015 15:00:00"),
        fdt.parseLocalDateTime("30/11/2015 11:00:00"),
        fdt.parseLocalDateTime("30/11/2015 13:00:00"),
        fdt.parseLocalDateTime("30/11/2015 15:00:00"),
        fdt.parseLocalDateTime("30/11/2015 17:00:00")
      ).map(PostgresBlacklist.partialDay("A", _, 1, global = true))

      val tt = timetable
      val aEntries = (0 until 7).map {
        case e if e < 5 => PostgresAssignmentEntry(e, "label", Set.empty[PostgresAssignmentEntryType])
        case e => PostgresAssignmentEntry(e, "label", Set.empty[PostgresAssignmentEntryType], e - 3)
      }.toSet
      val plan = PostgresAssignmentPlan(tt.labwork, aEntries.size, aEntries.size, aEntries)
      val groupSize = 6

      val expectedStart = Vector(
        fdt.parseLocalDateTime("19/10/2015 11:00:00"),
        fdt.parseLocalDateTime("26/10/2015 13:00:00"),
        fdt.parseLocalDateTime("02/11/2015 17:00:00"),
        fdt.parseLocalDateTime("16/11/2015 11:00:00"),
        fdt.parseLocalDateTime("07/12/2015 11:00:00"),
        fdt.parseLocalDateTime("11/01/2016 15:00:00"),
        fdt.parseLocalDateTime("08/02/2016 11:00:00")
      )

      val result = TimetableService.extrapolateTimetableByWeeks(tt, weeks, globalBlacklist2 ++ localBlacklist, plan, groupSize)

      checkAssertion(tt, plan, groupSize, expectedStart, result)

      val local = localBlacklist.map(b => b.date.toLocalDateTime(b.start)).toSet
      val other = result.map(e => e.date.toLocalDateTime(e.start)).toSet
      local.subsetOf(other) shouldBe false
    }

    "pass timetable entries when there are no blacklists" in {
      val entries = timetableDateEntries
      val blacklists = Vector.empty[PostgresBlacklist]

      val result = TimetableService.withoutBlacklists(entries, blacklists)

      assertEverything(entries, blacklists, result)(_ shouldBe blacklists.size)(_ shouldBe entries.size)
    }

    "apply blacklist dates on timetable entries" in {
      val entries = timetableDateEntries

      val blacklists = entries.slice(0, 10).map(asBlacklist) ++
        entries.slice(10, 20).map(asBlacklist) ++
        entries.slice(20, 30).map(asBlacklist)

      val result = TimetableService.withoutBlacklists(entries, blacklists)

      assertEverything(entries, blacklists, result)(_ shouldBe blacklists.size)(_ shouldBe entries.size - blacklists.size)
    }

    "apply blacklist dates even when they are tricky" in {
      val DAY_ONE = 1
      val DAY_TWO = 2
      val DAY_OTHER = 3
      val DAY_YET_ANOTHER = 4

      val entries = Vector(
        LocalDateTime.now.withDayOfMonth(DAY_ONE).withTime(9, 0, 0, 0), // 1. 9 - 10 -
        LocalDateTime.now.withDayOfMonth(DAY_ONE).withTime(10, 0, 0, 0), // 1. 10 - 11 -
        LocalDateTime.now.withDayOfMonth(DAY_ONE).withTime(11, 0, 0, 0), // 1. 11 - 12 -
        LocalDateTime.now.withDayOfMonth(DAY_ONE).withTime(12, 0, 0, 0), // 1. 12 - 13 +
        LocalDateTime.now.withDayOfMonth(DAY_TWO).withTime(9, 0, 0, 0), // 2. 9 - 11 -
        LocalDateTime.now.withDayOfMonth(DAY_TWO).withTime(12, 0, 0, 0), // 2. 12 - 14 +
        LocalDateTime.now.withDayOfMonth(DAY_TWO).withTime(13, 0, 0, 0), // 2. 13 - 15 +
        LocalDateTime.now.withDayOfMonth(DAY_TWO).withTime(18, 0, 0, 0), // 2. 18 - 20 +
        LocalDateTime.now.withDayOfMonth(DAY_OTHER).withTime(9, 0, 0, 0), // 3. 9 - 11 -
        LocalDateTime.now.withDayOfMonth(DAY_OTHER).withTime(14, 0, 0, 0), // 3. 14 - 16 -
        LocalDateTime.now.withDayOfMonth(DAY_YET_ANOTHER).withTime(14, 0, 0, 0) // 4. 14 - 16 -
      ) map { date =>
        TimetableDateEntry(Weekday.toDay(date.toLocalDate), date.toLocalDate, date.toLocalTime, date.toLocalTime.plusHours(if (date.getDayOfMonth == DAY_ONE) DAY_ONE else DAY_TWO), UUID.randomUUID, Set(UUID.randomUUID))
      }

      val blacklists = Vector(
        LocalDateTime.now.withDayOfMonth(DAY_ONE).withTime(8, 0, 0, 0), // 1. 8 - 9
        LocalDateTime.now.withDayOfMonth(DAY_ONE).withTime(11, 0, 0, 0), // 1. 11 - 12
        LocalDateTime.now.withDayOfMonth(DAY_ONE).withTime(15, 0, 0, 0), // 1. 15 - 16
        LocalDateTime.now.withDayOfMonth(DAY_ONE).withTime(9, 30, 0, 0), // 1. 9:30 - 10:30
        LocalDateTime.now.withDayOfMonth(DAY_TWO).withTime(10, 0, 0, 0), // 2. 10 - 11
        LocalDateTime.now.withDayOfMonth(DAY_OTHER).withTime(9, 0, 0, 0), // 3. 9 - 10
        LocalDateTime.now.withDayOfMonth(DAY_OTHER).withTime(16, 0, 0, 0) // 3. 16 - 17
      ).map(PostgresBlacklist.partialDay("", _, 1, global = true))

      val blacklists2 = Vector(
        LocalDate.now.withDayOfMonth(DAY_OTHER),
        LocalDate.now.withDayOfMonth(DAY_YET_ANOTHER)
      ).map(PostgresBlacklist.entireDay("", _, global = true))

      val result = TimetableService.withoutBlacklists(entries, blacklists ++ blacklists2)

      assertEverything(entries, blacklists, result)(_ shouldBe 4)(_ shouldBe 4)
    }
  }

  val fdt = DateTimeFormat.forPattern("dd/MM/yyyy HH:mm:ss")
  val ft = DateTimeFormat.forPattern("HH:mm:ss")
  val fd = DateTimeFormat.forPattern("dd/MM/yyyy")

  val weeks = Weeks.weeks(30)

  val profileWeek = (0 until 5).map(n => fd.parseDateTime("23/11/2015").plusDays(n)).toSet
  val christmas = (0 until 3 * 7).map(n => fd.parseDateTime("21/12/2015").plusDays(n)).toSet
  val globalBlacklist2 = {
    PostgresBlacklist.entireDay("Profil hoch 2", profileWeek.map(_.toLocalDate).toVector, global = true) ++
      PostgresBlacklist.entireDay("Weihnachten", christmas.map(_.toLocalDate).toVector, global = true)
  }

  private def timetable = {
    val tEntries = Set(
      PostgresTimetableEntry(Set(UUID.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("19/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("13:00:00")),
      PostgresTimetableEntry(Set(UUID.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("19/10/2015")).index, ft.parseLocalTime("13:00:00"), ft.parseLocalTime("15:00:00")),
      PostgresTimetableEntry(Set(UUID.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("19/10/2015")).index, ft.parseLocalTime("15:00:00"), ft.parseLocalTime("17:00:00")),
      PostgresTimetableEntry(Set(UUID.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("19/10/2015")).index, ft.parseLocalTime("17:00:00"), ft.parseLocalTime("19:00:00")),
      PostgresTimetableEntry(Set(UUID.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("23/10/2015")).index, ft.parseLocalTime("15:00:00"), ft.parseLocalTime("17:00:00"))
    )

    PostgresTimetable(UUID.randomUUID, tEntries, fd.parseLocalDate("19/10/2015"), Set.empty)
  }

  private def timetableDateEntries = (0 until 100).map { n =>
    val date = LocalDate.now.plusWeeks(n)
    val start = LocalTime.now.withHourOfDay(nextInt(16) + 1)
    val end = start.plusHours(nextInt(3) + 1)

    TimetableDateEntry(Weekday.toDay(date), date, start, end, UUID.randomUUID, Set(UUID.randomUUID))
  }.toVector

  private def asBlacklist(d: TimetableDateEntry): PostgresBlacklist = {
    PostgresBlacklist("", d.date, d.start, d.end, global = true)
  }

  private def assertEverything(entries: Vector[TimetableDateEntry],
                               blacklists: Vector[PostgresBlacklist],
                               result: Vector[TimetableDateEntry])
                              (countSizeAssert: (Int) => Unit)
                              (resultSizeAssert: (Int) => Unit) {
    val count = blacklists.count(l => entries.exists { d =>
      val lhs = new Interval(l.date.toDateTime(l.start), l.date.toDateTime(l.end))
      val rhs = new Interval(d.date.toDateTime(d.start), d.date.toDateTime(d.end))

      lhs overlaps rhs
    })

    countSizeAssert(count)

    result.map(toLocalDateTime).intersect(blacklists.map(toLocalDateTime)) shouldBe empty
    result.toSet.subsetOf(entries.toSet) shouldBe true

    resultSizeAssert(result.size)

    result.forall(a => blacklists.exists(toLocalDateTime(_).isEqual(toLocalDateTime(a)))) shouldBe false
    result.map(toLocalDateTime).sorted shouldBe sorted
  }

  private def checkAssertion(timetable: PostgresTimetable, plan: PostgresAssignmentPlan, groupSize: Int, expectedStart: Vector[LocalDateTime], result: Vector[TimetableDateEntry]) {
    import utils.LwmDateTime.localDateTimeOrd

    val sortedResult = result.map(toLocalDateTime).sorted

    result.size should be > timetable.entries.size
    result.size shouldBe groupSize * plan.entries.size
    sortedResult shouldBe sorted
    globalBlacklist2.map(b => b.date.toLocalDateTime(b.start)).toSet subsetOf result.map(e => e.date.toLocalDateTime(e.start)).toSet shouldBe false
    sortedResult.grouped(groupSize).forall(a => expectedStart.count(b => a.head.isEqual(b)) == 1) shouldBe true
    sortedResult.grouped(groupSize).foldLeft((true, expectedStart)) {
      case ((bool, vec), e) =>
        (bool && e.head.isEqual(vec.head), vec.tail)
    }._1 shouldBe true
  }
}
