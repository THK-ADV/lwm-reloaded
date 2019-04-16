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
        sqlTime.localTime shouldEqual this.localTime(localTime.getHourOfDay, localTime.getMinuteOfHour, localTime.getSecondOfMinute, localTime.getMillisOfSecond)
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
  }
}
