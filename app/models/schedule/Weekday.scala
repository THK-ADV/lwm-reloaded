package models.schedule

import org.joda.time.LocalDate
import org.joda.time.DateTimeConstants

object Weekday {

  implicit val ordering: Ordering[Weekday] = new Ordering[Weekday] {
    override def compare(x: Weekday, y: Weekday): Int = x compareTo y
  }

  def toDay(d: LocalDate): Weekday = toDay(d.getDayOfWeek)

  def toDay(i: Int): Weekday = (i - 1) % 7 match {
    case 0 => Monday
    case 1 => Tuesday
    case 2 => Wednesday
    case 3 => Thursday
    case 4 => Friday
    case 5 => Saturday
    case 6 => Sunday
    case _ => NoDay
  }
}

sealed abstract class Weekday(val index: Int) {

  import Weekday._

  override def equals(obj: scala.Any): Boolean = obj match {
    case w: Weekday => index == w.index
    case _ => false
  }

  def compareTo(z: Weekday): Int = index - z.index

  def sync(date: LocalDate): LocalDate = {
    if (index < date.getDayOfWeek)
      date.minusDays(date.getDayOfWeek - index)
    else
      date.plusDays(index - date.getDayOfWeek)
  }

  def minusDays(n: Int) = toDay(index - n)
  def plusDays(n: Int) = toDay(n + index)
}

case object NoDay extends Weekday(0)
case object Monday extends Weekday(DateTimeConstants.MONDAY)
case object Tuesday extends Weekday(DateTimeConstants.TUESDAY)
case object Wednesday extends Weekday(DateTimeConstants.WEDNESDAY)
case object Thursday extends Weekday(DateTimeConstants.THURSDAY)
case object Friday extends Weekday(DateTimeConstants.FRIDAY)
case object Saturday extends Weekday(DateTimeConstants.SATURDAY)
case object Sunday extends Weekday(DateTimeConstants.SUNDAY)