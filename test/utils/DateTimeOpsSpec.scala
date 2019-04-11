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
  }
}
