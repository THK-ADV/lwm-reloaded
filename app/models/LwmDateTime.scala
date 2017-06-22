package models

import java.sql.{Date, Time, Timestamp}
import java.util.{Calendar, Locale}

import org.joda.time.{DateTime, LocalDate, LocalDateTime, LocalTime}
import org.joda.time.format.DateTimeFormat
import play.api.libs.json.{JsString, Writes}

object LwmDateTime {

  def sqlDateNow: Date = new Date(Calendar.getInstance.getTimeInMillis)
  def sqlTimeNow: Time = new Time(sqlDateNow.getTime)

  implicit class LocalDateConverter(val date: LocalDate) {
    def string: String = date.toDateTimeAtStartOfDay.getMillis.toString
    def sqlDate: Date = date.string.sqlDate
  }

  implicit class StringDateConverter(val string: String) {
    def sqlDate: Date = new Date(string.toLong)
    def sqlTime: Time = new Time(string.toLong)

    def localDate: LocalDate = LocalDate.parse(string, DateTimeFormat.forPattern(datePattern))
  }

  implicit class LocalTimeConverter(val time: LocalTime) {
    def string: String = time.toDateTimeToday.getMillis.toString
    def sqlTime: Time = time.string.sqlTime
  }

  implicit class TimeConverter(val time: Time) {
    def string: String = time.localTime.string
    def localTime: LocalTime = new LocalTime(time.getTime)
  }

  implicit class DateTimeConverter(val date: DateTime) {
    def timestamp: Timestamp= new Timestamp(date.getMillis)
  }

  implicit class SqlDateConverter(val date: Date) {
    def string: String = date.localDate.string
    def localDate: LocalDate = new LocalDate(date.getTime)
  }

  implicit class SqlTimestampConverter(val timestamp: Timestamp) {
    def dateTime: DateTime = new DateTime(timestamp.getTime)
  }

  lazy val pattern = "yyyy-MM-dd'T'HH:mm"
  lazy val datePattern = "yyyy-MM-dd"
  lazy val formatter = DateTimeFormat.forPattern(pattern)

  implicit def writes: Writes[DateTime] = Writes(a => JsString(a.toString(formatter)))

  def toLocalTime(date: Date): LocalTime = new LocalTime(date.getTime)

  def toDateTime(string: String) = DateTime.parse(string, formatter)

  def isEqual(inputDates: Set[String], outputDates: Set[DateTime]) = {
    inputDates.map(toDateTime).diff(outputDates.map(date => DateTime.parse(date.toString(formatter)))).isEmpty
  }

  implicit val localTimeOrd: Ordering[LocalTime] = new Ordering[LocalTime] {
    override def compare(x: LocalTime, y: LocalTime): Int = x.compareTo(y)
  }

  implicit val localDateOrd: Ordering[LocalDate] = new Ordering[LocalDate] {
    override def compare(x: LocalDate, y: LocalDate): Int = x.compareTo(y)
  }

  implicit val localDateTimeOrd: Ordering[LocalDateTime] = new Ordering[LocalDateTime] {
    override def compare(x: LocalDateTime, y: LocalDateTime): Int = x.compareTo(y)
  }

  implicit val dateTimeOrd: Ordering[DateTime] = new Ordering[DateTime] {
    override def compare(x: DateTime, y: DateTime): Int = x.compareTo(y)
  }
}
