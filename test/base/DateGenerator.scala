package base

import org.joda.time.{DateTime, DateTimeZone, LocalDate, LocalTime}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.Suite

trait DateGenerator {
  self: Suite =>

  protected def localDate(year: Int, month: Int, day: Int): LocalDate = LocalDate.now
    .withYear(year)
    .withMonthOfYear(month)
    .withDayOfMonth(day)

  protected def localTime(hour: Int, minute: Int = 0, second: Int = 0, milliSeconds: Int = 0): LocalTime = LocalTime.now
    .withHourOfDay(hour)
    .withMinuteOfHour(minute)
    .withSecondOfMinute(second)
    .withMillisOfSecond(milliSeconds)

  protected def dateTime(year: Int, month: Int, day: Int, hour: Int, minute: Int, second: Int, milliSeconds: Int): DateTime = DateTime.now
    .withYear(year)
    .withMonthOfYear(month)
    .withDayOfMonth(day)
    .withHourOfDay(hour)
    .withMinuteOfHour(minute)
    .withSecondOfMinute(second)
    .withMillisOfSecond(milliSeconds)

  def dates: Gen[DateTime] = {
    for {
      year <- Gen.choose(1970, 2019)
      month <- Gen.choose(1, 12)
      day <- Gen.choose(1, 28)
      hour <- Gen.choose(1, 23)
      minute <- Gen.choose(1, 59)
      second <- Gen.choose(1, 59)
    } yield dateTime(year, month, day, hour, minute, second, 0)
  }

  def randomLocalDate = dates.sample.get.toLocalDate

  def randomLocalTime = dates.sample.get.toLocalTime

  implicit val arbitraryDateTime: Arbitrary[DateTime] = Arbitrary(dates)

  implicit val arbitraryLocalTime: Arbitrary[LocalTime] = Arbitrary(dates.map(_.toLocalTime))

  implicit val arbitraryLocalDate: Arbitrary[LocalDate] = Arbitrary(dates.map(_.toLocalDate))
}
