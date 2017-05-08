package models

import java.sql.{Date, Time, Timestamp}

import org.joda.time.{DateTime, LocalDate, LocalDateTime, LocalTime}
import org.joda.time.format.DateTimeFormat
import play.api.libs.json.{JsString, Writes}

object LwmDateTime {

  implicit class LocalDateConverter(val date: LocalDate) {
    def sqlDate: Date = Date.valueOf(date.toString)
  }

  implicit class LocalTimeConverter(val time: LocalTime) {
    def sqlTime: Time = Time.valueOf(time.toString)
  }

  implicit class DateTimeConverter(val date: DateTime) {
    def timestamp: Timestamp= new Timestamp(date.getMillis)
  }

  implicit class SqlDateConverter(val date: Date) {
    def localDate: LocalDate = new LocalDate(date.getTime)
  }

  implicit class SqlTimestampConverter(val timestamp: Timestamp) {
    def dateTime: DateTime = new DateTime(timestamp.getTime)
  }

  lazy val pattern = "yyyy-MM-dd'T'HH:mm"
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
