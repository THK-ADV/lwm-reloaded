package utils

import java.sql.{Date, Time, Timestamp}
import java.util.Calendar

import models.genesis.ScheduleEntryGen
import models.Blacklist
import models.helper.TimetableDateEntry
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}
import org.joda.time.{DateTime, LocalDate, LocalDateTime, LocalTime}
import play.api.libs.json._

import scala.util.{Failure, Success, Try}

object LwmDateTime {

  implicit class LocalDateConverter(val date: LocalDate) {
    def sqlDate: Date = date.stringMillis.sqlDateFromMillis

    def stringMillis: String = date.toDateTimeAtStartOfDay.getMillis.toString
  }

  implicit class StringDateConverter(val string: String) {
    def sqlDateFromMillis: Date = new Date(string.toLong)

    def sqlTimeFromMillis: Time = new Time(string.toLong)

    def sqlDateFromPattern: Date = string.localDate.sqlDate

    def sqlTimeFromPattern: Time = string.localTime.sqlTime

    def localDate: LocalDate = LocalDate.parse(string, dateFormatter)

    def localTime: LocalTime = LocalTime.parse(string, timeFormatter)
  }

  implicit class LocalTimeConverter(val time: LocalTime) {
    def sqlTime: Time = time.stringMillis.sqlTimeFromMillis

    def stringMillis: String = time.toDateTimeToday.getMillis.toString
  }

  implicit class TimeConverter(val time: Time) {
    def stringMillis: String = time.localTime.stringMillis

    def localTime: LocalTime = new LocalTime(time.getTime)
  }

  implicit class DateTimeConverter(val date: DateTime) {
    def timestamp: Timestamp = new Timestamp(date.getMillis)
  }

  implicit class SqlDateConverter(val date: Date) {
    def stringMillis: String = date.localDate.stringMillis

    def localDate: LocalDate = new LocalDate(date.getTime)
  }

  implicit class SqlTimestampConverter(val timestamp: Timestamp) {
    def dateTime: DateTime = new DateTime(timestamp.getTime)
  }

  lazy val pattern = "yyyy-MM-dd'T'HH:mm"
  lazy val datePattern = "yyyy-MM-dd"
  lazy val timePattern = "HH:mm:ss"
  lazy val formatter: DateTimeFormatter = DateTimeFormat.forPattern(pattern)
  lazy val dateFormatter: DateTimeFormatter = DateTimeFormat.forPattern(datePattern)
  lazy val timeFormatter: DateTimeFormatter = DateTimeFormat.forPattern(timePattern)

  def sqlTimeNow: Time = new Time(sqlDateNow.getTime)

  def sqlDateNow: Date = new Date(Calendar.getInstance.getTimeInMillis)

  def toLocalTime(date: Date): LocalTime = new LocalTime(date.getTime)

  def toLocalDateTime(entry: TimetableDateEntry): LocalDateTime = {
    entry.date.toLocalDateTime(entry.start)
  }

  def toLocalDateTime(entry: ScheduleEntryGen): LocalDateTime = {
    entry.date.toLocalDateTime(entry.start)
  }

  def toLocalDateTime(bl: Blacklist): LocalDateTime = {
    bl.date.toLocalDateTime(bl.start)
  }

  def isEqual(inputDates: Set[String], outputDates: Set[DateTime]): Boolean = {
    inputDates.map(toDateTime).diff(outputDates.map(date => DateTime.parse(date.toString(formatter)))).isEmpty
  }

  def toDateTime(string: String): DateTime = DateTime.parse(string, formatter)

  implicit val localTimeOrd: Ordering[LocalTime] = (x: LocalTime, y: LocalTime) => x.compareTo(y)

  implicit val localDateOrd: Ordering[LocalDate] = (x: LocalDate, y: LocalDate) => x.compareTo(y)

  implicit val localDateTimeOrd: Ordering[LocalDateTime] = (x: LocalDateTime, y: LocalDateTime) => x.compareTo(y)

  implicit val dateTimeOrd: Ordering[DateTime] = (x: DateTime, y: DateTime) => x.compareTo(y)

  implicit val writeDateTime: Writes[DateTime] = Writes(a => JsString(a.toString(formatter)))

  implicit val writeLocalDate: Writes[LocalDate] = Writes(a => JsString(a.toString(dateFormatter)))

  implicit val writeLocalTime: Writes[LocalTime] = Writes(a => JsString(a.toString(timeFormatter)))

  implicit val readLocalDate: Reads[LocalDate] = Reads(js => jsResult(Try(dateFormatter.parseLocalDate(js.toString()))))

  implicit val readLocalTime: Reads[LocalTime] = Reads(js => jsResult(Try(timeFormatter.parseLocalTime(js.toString()))))

  implicit val readDateTime: Reads[DateTime] = Reads(js => jsResult(Try(formatter.parseDateTime(js.toString()))))

  private def jsResult[A](a: Try[A]): JsResult[A] = a match {
    case Success(s) => JsSuccess(s)
    case Failure(e) => JsError(e.getLocalizedMessage)
  }
}
