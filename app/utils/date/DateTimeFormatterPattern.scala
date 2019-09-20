package utils.date

import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat

import scala.util.Try

trait DateTimeFormatterPattern {
  lazy val dateTimePattern = "yyyy-MM-dd'T'HH:mm"
  lazy val datePattern = "yyyy-MM-dd"
  lazy val timePattern = "HH:mm:ss"
  lazy val dateTimeFormatter = DateTimeFormat.forPattern(dateTimePattern)
  lazy val dateFormatter = DateTimeFormat.forPattern(datePattern)
  lazy val timeFormatter = DateTimeFormat.forPattern(timePattern)

  def localDate(pattern: String): Option[LocalDate] = Try(dateFormatter.parseLocalDate(pattern)).toOption
}
