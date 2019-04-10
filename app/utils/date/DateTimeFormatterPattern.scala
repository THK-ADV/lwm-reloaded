package utils.date

import org.joda.time.format.DateTimeFormat

trait DateTimeFormatterPattern {
  lazy val dateTimePattern = "yyyy-MM-dd'T'HH:mm"
  lazy val datePattern = "yyyy-MM-dd"
  lazy val timePattern = "HH:mm:ss"
  lazy val dateTimeFormatter = DateTimeFormat.forPattern(dateTimePattern)
  lazy val dateFormatter = DateTimeFormat.forPattern(datePattern)
  lazy val timeFormatter = DateTimeFormat.forPattern(timePattern)
}
