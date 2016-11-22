package services

import java.util.UUID

import akka.util.Timeout
import base.TestBaseDefinition
import models.{TimetableDateEntry, User, Weekday}
import org.joda.time.{DateTime, LocalDate, LocalTime}
import org.scalatest.WordSpec

import scala.concurrent.Await
import scala.concurrent.duration._

class BlacklistServiceSpec extends WordSpec with TestBaseDefinition {

  import models.TimetableDateEntry._
  import models.LwmDateTime.localDateTimeOrd
  import scala.util.Random.nextInt

  val blacklistService = new BlacklistService

  def toDateTime(entry: TimetableDateEntry): DateTime = {
    entry.date.toDateTime(entry.start)
  }

  "A BlacklistServiceSpec" should {

    def timetableDateEntries = (0 until 100).map { n =>
      val date = LocalDate.now.plusWeeks(n)
      val start = LocalTime.now.withHourOfDay(nextInt(19))
      val end = start.plusHours(nextInt(3))

      TimetableDateEntry(Weekday.toDay(date), date, start, end, UUID.randomUUID, Set(User.randomUUID))
    }.toVector

    "pass timetable entries when there are no blacklists" in {
      val entries = timetableDateEntries
      val blacklists = Set.empty[DateTime]

      val result = blacklistService.filterBy(entries, blacklists)

      blacklists.count(l => entries.exists(_.start.isEqual(l.toLocalTime))) shouldBe blacklists.size
      result.map(toLocalDateTime).intersect(blacklists.map(_.toLocalDateTime).toVector) shouldBe empty
      result.toSet.subsetOf(entries.toSet) shouldBe true
      result shouldBe entries
      result.size shouldBe entries.size
      result.forall(a => blacklists.exists(_.toLocalDateTime.isEqual(toLocalDateTime(a)))) shouldBe false
      result.map(toLocalDateTime).sorted shouldBe sorted
    }

    "apply blacklist dates on timetable entries" in {
      val entries = timetableDateEntries

      val blacklists = entries.slice(0, 10).map(toDateTime).toSet ++
        entries.slice(10, 20).map(toDateTime).toSet ++
        entries.slice(20, 30).map(toDateTime).toSet

      val result = blacklistService.filterBy(entries, blacklists)

      blacklists.count(l => entries.exists(_.start.isEqual(l.toLocalTime))) shouldBe blacklists.size
      result.map(toLocalDateTime).intersect(blacklists.map(_.toLocalDateTime).toVector) shouldBe empty
      result.toSet.subsetOf(entries.toSet) shouldBe true
      result.size should be < entries.size
      result.size shouldBe entries.size - blacklists.size
      result.forall(a => blacklists.exists(_.toLocalDateTime.isEqual(toLocalDateTime(a)))) shouldBe false
      result.map(toLocalDateTime).sorted shouldBe sorted
    }

    "apply blacklist dates even when they are tricky" in {
      val DAY_ONE = 1
      val DAY_TWO = 2
      val DAY_OTHER = 3

      val entries = Vector(
        DateTime.now.withDayOfMonth(DAY_ONE).withTime(9, 0, 0, 0),
        DateTime.now.withDayOfMonth(DAY_ONE).withTime(10, 0, 0, 0),
        DateTime.now.withDayOfMonth(DAY_ONE).withTime(11, 0, 0, 0),
        DateTime.now.withDayOfMonth(DAY_ONE).withTime(12, 0, 0, 0),
        DateTime.now.withDayOfMonth(DAY_TWO).withTime(9, 0, 0, 0)
      ) map { date =>
        TimetableDateEntry(Weekday.toDay(date.toLocalDate), date.toLocalDate, date.toLocalTime, date.toLocalTime.plusHours(if (date.getDayOfMonth == DAY_ONE) DAY_ONE else DAY_TWO), UUID.randomUUID, Set(UUID.randomUUID))
      }

      val blacklists = Set(
        DateTime.now.withDayOfMonth(DAY_ONE).withTime(8, 0, 0, 0),
        DateTime.now.withDayOfMonth(DAY_ONE).withTime(11, 0, 0, 0),
        DateTime.now.withDayOfMonth(DAY_ONE).withTime(15, 0, 0, 0),
        DateTime.now.withDayOfMonth(DAY_ONE).withTime(9, 30, 0, 0),
        DateTime.now.withDayOfMonth(DAY_TWO).withTime(10, 0, 0, 0),
        DateTime.now.withDayOfMonth(DAY_OTHER).withTime(9, 0, 0, 0),
        DateTime.now.withDayOfMonth(DAY_OTHER + 1).withTime(16, 0, 0, 0)
      )

      val result = blacklistService.filterBy(entries, blacklists)

      result.map(toLocalDateTime).exists(_.getDayOfMonth == DAY_TWO) shouldBe false
      result.map(toLocalDateTime).count(_.getDayOfMonth == DAY_ONE) shouldBe entries.map(toLocalDateTime).count(_.getDayOfMonth == DAY_ONE) - DAY_TWO
      blacklists.count(l => entries.exists(_.start.isEqual(l.toLocalTime))) should be <= blacklists.size
      result.map(toLocalDateTime).intersect(blacklists.map(_.toLocalDateTime).toVector) shouldBe empty
      result.toSet.subsetOf(entries.toSet) shouldBe true
      result.size should be < entries.size
      result.forall(a => blacklists.exists(_.toLocalDateTime.isEqual(toLocalDateTime(a)))) shouldBe false
      result.map(toLocalDateTime).sorted shouldBe sorted
    }

    "fetch blacklists from an external api" in {
      val timeout = Timeout(5 seconds)
      val year = DateTime.now.getYear.toString

      val future = blacklistService.fetchByYear(year)
      val result = Await.result(future, timeout.duration)

      result.label shouldBe BlacklistService.legalHolidayLabel(year)
      result.dates should not be empty
      result.dates.forall(_.getYear == year.toInt) shouldBe true
      result.dates should contain(DateTime.now.withDayOfYear(1).withTimeAtStartOfDay)
    }
  }
}
