package utils.date

import java.sql.{Date, Time, Timestamp}

import models.genesis.ScheduleEntryGen
import models.helper.TimetableDateEntry
import org.joda.time.{DateTime, LocalDate, LocalDateTime, LocalTime}

object DateTimeOps extends DateTimeFormatterPattern {

  implicit class LocalDateConverter(val date: LocalDate) {
    def sqlDate: Date = Date.valueOf(date.toString(datePattern))
  }

  implicit class LocalTimeConverter(val time: LocalTime) {
    def sqlTime: Time = Time.valueOf(time.toString(timePattern))
  }

  implicit class DateTimeConverter(val date: DateTime) {
    def timestamp: Timestamp = new Timestamp(date.getMillis)
  }

  implicit class SqlTimeConverter(val time: Time) {
    def localTime: LocalTime = new LocalTime(time.getTime)
  }

  implicit class SqlDateConverter(val date: Date) {
    def localDate: LocalDate = new LocalDate(date.getTime)
  }

  implicit class SqlTimestampConverter(val timestamp: Timestamp) {
    def dateTime: DateTime = new DateTime(timestamp.getTime)
  }

  def toLocalDateTime(entry: TimetableDateEntry): LocalDateTime = {
    entry.date.toLocalDateTime(entry.start)
  }

  def toLocalDateTime(entry: ScheduleEntryGen): LocalDateTime = {
    entry.date.toLocalDateTime(entry.start)
  }

  implicit val localTimeOrd: Ordering[LocalTime] = (x: LocalTime, y: LocalTime) => x.compareTo(y)

  implicit val localDateOrd: Ordering[LocalDate] = (x: LocalDate, y: LocalDate) => x.compareTo(y)

  implicit val localDateTimeOrd: Ordering[LocalDateTime] = (x: LocalDateTime, y: LocalDateTime) => x.compareTo(y)

  implicit val dateTimeOrd: Ordering[DateTime] = (x: DateTime, y: DateTime) => x.compareTo(y)
}
