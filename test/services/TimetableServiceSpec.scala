package services

import base.TestBaseDefinition
import models.labwork._
import models.semester.Blacklist
import models.users.User
import models._
import models.labwork.Weekday
import org.joda.time.{DateTime, LocalDateTime, Weeks}
import org.joda.time.format.DateTimeFormat
import org.scalatest.WordSpec
import org.mockito.Matchers._
import org.mockito.Mockito._

import scala.util.Success

class TimetableServiceSpec extends WordSpec with TestBaseDefinition {

  import models.labwork.TimetableDateEntry._

  val blacklistService = new BlacklistService
  val timetableService = new TimetableService(blacklistService)

  val fdt = DateTimeFormat.forPattern("dd/MM/yyyy HH:mm:ss")
  val ft = DateTimeFormat.forPattern("HH:mm:ss")
  val fd = DateTimeFormat.forPattern("dd/MM/yyyy")

  val weeks = Weeks.weeks(30)

  val profileWeek = (0 until 5).map(n => fd.parseDateTime("23/11/2015").plusDays(n)).toSet
  val christmas = (0 until 3 * 7).map(n => fd.parseDateTime("21/12/2015").plusDays(n)).toSet
  val globalBlacklist = Set(Blacklist("Profil hoch 2", profileWeek), Blacklist("Weihnachten", christmas))
  val members = (0 until 20).map(_ => User.randomUUID).toSet

  def timetable(localBlacklist: Set[DateTime] = Set.empty) = {
    val tEntries = Set(
      TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("19/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("13:00:00")),
      TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("19/10/2015")).index, ft.parseLocalTime("13:00:00"), ft.parseLocalTime("15:00:00")),
      TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("19/10/2015")).index, ft.parseLocalTime("15:00:00"), ft.parseLocalTime("17:00:00")),
      TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("19/10/2015")).index, ft.parseLocalTime("17:00:00"), ft.parseLocalTime("19:00:00")),
      TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("23/10/2015")).index, ft.parseLocalTime("15:00:00"), ft.parseLocalTime("17:00:00"))
    )

    Timetable(Labwork.randomUUID, tEntries, fd.parseLocalDate("19/10/2015"), localBlacklist ++ globalBlacklist.flatMap(_.dates))
  }

  "A TimetableService" should {

    "extrapolate further entries based on frontend's timetable protocol template and assignment plan where some assignments takes more than one week with blacklists applied" in {
      val tt = timetable()
      val aEntries = (0 until 7).map {
        case e if e < 5 => AssignmentEntry(e, "label", Set.empty[AssignmentEntryType])
        case e => AssignmentEntry(e, "label", Set.empty[AssignmentEntryType], e - 3)
      }.toSet
      val plan = AssignmentPlan(tt.labwork, aEntries.size, aEntries.size, aEntries)
      val groups = (0 until 6).map(n => Group(n.toString, tt.labwork, members)).toSet

      val expectedStart = Vector(
        fdt.parseDateTime("19/10/2015 11:00:00"),
        fdt.parseDateTime("26/10/2015 13:00:00"),
        fdt.parseDateTime("02/11/2015 15:00:00"),
        fdt.parseDateTime("09/11/2015 17:00:00"),
        fdt.parseDateTime("20/11/2015 15:00:00"),
        fdt.parseDateTime("14/12/2015 13:00:00"),
        fdt.parseDateTime("29/01/2016 15:00:00")
      )

      val result = timetableService.extrapolateTimetableByWeeks(tt, weeks, plan, groups)

      checkAssertion(tt, plan, groups, expectedStart, result)
    }

    "extrapolate further entries based on frontend's timetable protocol template and assignment plan where each assignment takes 2 weeks with blacklists applied" in {
      val tt = timetable()
      val aEntries = (0 until 5).map(AssignmentEntry(_, "label", Set.empty[AssignmentEntryType], 2)).toSet
      val plan = AssignmentPlan(tt.labwork, aEntries.size, aEntries.size, aEntries)
      val groups = (0 until 6).map(n => Group(n.toString, tt.labwork, members)).toSet

      val expectedStart = Vector(
        fdt.parseDateTime("19/10/2015 11:00:00"),
        fdt.parseDateTime("02/11/2015 15:00:00"),
        fdt.parseDateTime("20/11/2015 15:00:00"),
        fdt.parseDateTime("14/12/2015 13:00:00"),
        fdt.parseDateTime("18/01/2016 17:00:00")
      )

      val result = timetableService.extrapolateTimetableByWeeks(tt, weeks, plan, groups)

      checkAssertion(tt, plan, groups, expectedStart, result)
    }

    def checkAssertion(timetable: Timetable, plan: AssignmentPlan, groups: Set[Group], expectedStart: Vector[DateTime], result: Vector[TimetableDateEntry]): Unit = {
      val sortedResult = result.map(toLocalDateTime).sorted

      result.size should be > timetable.entries.size
      result.size shouldBe groups.size * plan.entries.size
      sortedResult shouldBe sorted
      globalBlacklist.forall(a => a.dates.subsetOf(result.map(_.date.toDateTimeAtCurrentTime).toSet)) shouldBe false
      sortedResult.grouped(groups.size).forall(a => expectedStart.count(b => a.head.isEqual(b.toLocalDateTime)) == 1) shouldBe true
      sortedResult.grouped(groups.size).foldLeft((true, expectedStart)) {
        case ((bool, vec), e) =>
          (bool && e.head.isEqual(vec.head.toLocalDateTime), vec.tail)
      }._1 shouldBe true
    }

    "extrapolate further entries based on frontend's timetable protocol template and assignment plan where some assignments takes more than one week with local and global blacklists applied" in {
      val localBlacklist = Set(
        fdt.parseDateTime("30/10/2015 15:00:00"),
        fdt.parseDateTime("06/11/2015 15:00:00"),
        fdt.parseDateTime("30/11/2015 11:00:00"),
        fdt.parseDateTime("30/11/2015 13:00:00"),
        fdt.parseDateTime("30/11/2015 15:00:00"),
        fdt.parseDateTime("30/11/2015 17:00:00")
      )

      val tt = timetable(localBlacklist)
      val aEntries = (0 until 7).map {
        case e if e < 5 => AssignmentEntry(e, "label", Set.empty[AssignmentEntryType])
        case e => AssignmentEntry(e, "label", Set.empty[AssignmentEntryType], e - 3)
      }.toSet
      val plan = AssignmentPlan(tt.labwork, aEntries.size, aEntries.size, aEntries)
      val groups = (0 until 6).map(n => Group(n.toString, tt.labwork, members)).toSet

      val expectedStart = Vector(
        fdt.parseDateTime("19/10/2015 11:00:00"),
        fdt.parseDateTime("26/10/2015 13:00:00"),
        fdt.parseDateTime("02/11/2015 17:00:00"),
        fdt.parseDateTime("16/11/2015 11:00:00"),
        fdt.parseDateTime("07/12/2015 11:00:00"),
        fdt.parseDateTime("11/01/2016 15:00:00"),
        fdt.parseDateTime("08/02/2016 11:00:00")
      )

      val result = timetableService.extrapolateTimetableByWeeks(tt, weeks, plan, groups)

      checkAssertion(tt, plan, groups, expectedStart, result)
      localBlacklist.subsetOf(result.map(e => e.date.toDateTime(e.start)).toSet) shouldBe false
    }
  }
}
