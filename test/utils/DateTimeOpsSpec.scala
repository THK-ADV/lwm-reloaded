package utils

import java.sql.{Date, Time}

import base.{DateGenerator, TestBaseDefinition}
import org.joda.time.{DateTime, LocalDate, LocalTime}
import org.scalatest.WordSpec
import org.scalatest.prop.PropertyChecks

final class DateTimeOpsSpec extends WordSpec with TestBaseDefinition with DateGenerator with PropertyChecks {

  "A DateTimeOpsSpec" should {

    "test LocalDate to sql.Date conversion" in {
      import utils.date.DateTimeOps.{LocalDateConverter, SqlDateConverter}

      forAll { localDate: LocalDate =>
        val sqlDate = localDate.sqlDate

        sqlDate shouldEqual Date.valueOf(s"${localDate.getYear}-${localDate.getMonthOfYear}-${localDate.getDayOfMonth}")
        sqlDate.localDate shouldEqual this.localDate(localDate.getYear, localDate.getMonthOfYear, localDate.getDayOfMonth)
      }
    }

    "test LocalTime to sql.Time conversions" in {
      import utils.date.DateTimeOps.{LocalTimeConverter, SqlTimeConverter}

      forAll { localTime: LocalTime =>
        val sqlTime = localTime.sqlTime

        sqlTime shouldEqual Time.valueOf(s"${localTime.getHourOfDay}:${localTime.getMinuteOfHour}:${localTime.getSecondOfMinute}")
        sqlTime.localTime shouldEqual this.localTime(localTime.getHourOfDay, localTime.getMinuteOfHour, localTime.getSecondOfMinute, localTime.getMillisOfSecond).withMillisOfSecond(0)
      }
    }

    "test DateTime to sql.Timestamp conversions" in {
      import utils.date.DateTimeOps.{DateTimeConverter, SqlTimestampConverter}

      forAll { dateTime: DateTime =>
        val timestamp = dateTime.timestamp

        timestamp.getTime shouldEqual dateTime.getMillis
        timestamp.dateTime shouldEqual this.dateTime(dateTime.getYear, dateTime.getMonthOfYear, dateTime.getDayOfMonth, dateTime.getHourOfDay, dateTime.getMinuteOfHour, dateTime.getSecondOfMinute, dateTime.getMillisOfSecond)
      }
    }

    "test String to LocalDate conversions" in {
      import utils.date.DateTimeOps.StringConverter

      "1990-02-05".localDate.success.value shouldEqual localDate(1990, 2, 5)
      "2008-12-31".localDate.success.value shouldEqual localDate(2008, 12, 31)
      "2010-10-17".localDate.success.value shouldEqual localDate(2010, 10, 17)
      "2010:10-17".localDate.failure.exception shouldBe a[IllegalArgumentException]
    }

    "test String to LocalTime conversions" in {
      import utils.date.DateTimeOps.StringConverter

      "13:37:00".localTime.success.value shouldEqual localTime(13, 37, 0, 0)
      "18:15:38".localTime.success.value shouldEqual localTime(18, 15, 38, 0)
      "17:01:08".localTime.success.value shouldEqual localTime(17, 1, 8, 0)
      "14:05".localTime.failure.exception shouldBe a[IllegalArgumentException]
    }

    "create a range from start until end" in {
      import utils.date.DateTimeOps.mapUntil

      mapUntil(
        localDate(2019, 1, 1),
        localDate(2019, 1, 31),
        identity
      ).size shouldBe 30
    }

    "create a range from start to end" in {
      import utils.date.DateTimeOps.mapTo

      mapTo(
        localDate(2019, 1, 1),
        localDate(2019, 1, 31),
        identity
      ).size shouldBe 31
    }

    "not create a range if start until end are the same" in {
      import utils.date.DateTimeOps.mapUntil

      mapUntil(
        localDate(2019, 1, 1),
        localDate(2019, 1, 1),
        identity
      ).size shouldBe 0
    }

    "create a range of 1 if start to end are the same" in {
      import utils.date.DateTimeOps.mapTo

      mapTo(
        localDate(2019, 1, 1),
        localDate(2019, 1, 1),
        identity
      ).size shouldBe 1
    }

    "fail creating a range if start is after end" in {
      import utils.date.DateTimeOps.{mapTo, mapUntil}

      val until = mapUntil(
        localDate(2019, 1, 5),
        localDate(2019, 1, 1),
        identity
      )

      val to = mapTo(
        localDate(2019, 1, 5),
        localDate(2019, 1, 1),
        identity
      )

      until.size shouldBe 0
      to.size shouldBe 0
    }
  }
}
