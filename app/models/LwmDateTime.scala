package models

import org.joda.time.{DateTime, LocalDate, LocalDateTime, LocalTime}
import org.joda.time.format.DateTimeFormat
import play.api.libs.json.{JsString, Writes}

object LwmDateTime {

  lazy val pattern = "yyyy-MM-dd'T'HH:mm"
  lazy val formatter = DateTimeFormat.forPattern(pattern)

  implicit def writes: Writes[DateTime] = Writes(a => JsString(a.toString(formatter)))

  def toDateTime(dateTime: String) = DateTime.parse(dateTime, formatter)
  def toLocalDate(localDate: String) = LocalDate.parse(localDate, DateTimeFormat.forPattern("yyyy-MM-dd"))
  def toLocalTime(localTime: String) = LocalTime.parse(localTime, DateTimeFormat.forPattern("HH:mm"))

  def isEqual(inputDates: Set[String], outputDates: Set[DateTime]) = {
    val inputDateTimes = inputDates.map(s => toDateTime(s).toInstant)
    val outputDateTimes = outputDates.map(_.toInstant)
    val diff = inputDateTimes.diff(outputDateTimes) ++ outputDateTimes.diff(inputDateTimes)

    diff.isEmpty
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
