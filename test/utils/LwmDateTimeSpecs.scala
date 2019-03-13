package utils

import java.sql.{Date, Time, Timestamp}

import base.{DateGenerator, TestBaseDefinition}
import org.joda.time._
import org.scalatest.WordSpec
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.Json

final class LwmDateTimeSpecs extends WordSpec with TestBaseDefinition with PropertyChecks with DateGenerator {
  import utils.LwmDateTime._

  "A LwmDateTimeSpecs" should {

    "convert from joda LocalDate, -Time and -DateTime, to String and SqlDate, -Time and Timestamp back and forth" in {
      val dateStringPattern = "2017-02-05"
      val timeStringPattern = "11:00:00"

      val localDate = LocalDate.parse(dateStringPattern, dateFormatter)
      val localTime = LocalTime.parse(timeStringPattern, timeFormatter)
      val dateTime = DateTime.now

      val fullLocalDate = LocalDateTime.parse(dateStringPattern + "T00:00", dateTimeFormatter)
      val fullLocalTime = LocalDateTime.now.withHourOfDay(11).withMinuteOfHour(0).withSecondOfMinute(0).withMillisOfSecond(0)

      val sqlDate = localDate.sqlDate
      val sqlTime = localTime.sqlTime
      val timestamp = dateTime.timestamp

      localDate.stringMillis shouldBe fullLocalDate.toDateTime.getMillis.toString
      sqlDate shouldBe Date.valueOf(dateStringPattern)

      localTime.stringMillis shouldBe fullLocalTime.toDateTime.getMillis.toString
      sqlTime shouldBe new Time(fullLocalTime.toDateTime.getMillis)

      sqlTime.localTime shouldBe localTime
      sqlDate.localDate shouldBe localDate

      sqlTime.stringMillis shouldBe localTime.stringMillis
      sqlDate.stringMillis shouldBe localDate.stringMillis

      localTime.stringMillis.sqlTimeFromMillis shouldBe sqlTime
      localDate.stringMillis.sqlDateFromMillis shouldBe sqlDate

      sqlDate.stringMillis.sqlDateFromMillis shouldBe sqlDate
      sqlTime.stringMillis.sqlTimeFromMillis shouldBe sqlTime

      timestamp shouldBe new Timestamp(dateTime.getMillis)
      timestamp.dateTime shouldBe dateTime

      dateStringPattern.sqlDateFromPattern shouldBe sqlDate
      timeStringPattern.sqlTimeFromPattern shouldBe sqlTime
    }

    "convert different joda.DateTimes to and from json" in {
      forAll { dateTime: DateTime =>
        Json.toJson(dateTime).as[String] shouldBe dateTime.toString(dateTimeFormatter)
      }
    }

    "convert different joda.LocalTime to and from json" in {
      forAll { localTime: LocalTime =>
        Json.toJson(localTime).as[String] shouldBe localTime.toString(timeFormatter)
      }
    }

    "convert different joda.LocalDate to and from json" in {
      forAll { localDate: LocalDate =>
        Json.toJson(localDate).as[String] shouldBe localDate.toString(dateFormatter)
      }
    }
  }
}
