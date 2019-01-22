package models

import java.util.UUID

import org.joda.time.{LocalDate, LocalDateTime, LocalTime}
import play.api.libs.json._

case class Blacklist(label: String, date: LocalDate, start: LocalTime, end: LocalTime, global: Boolean, id: UUID = UUID.randomUUID) extends UniqueEntity

case class BlacklistProtocol(label: String, date: String, start: String, end: String, global: Boolean)

object Blacklist {
  import utils.LwmDateTime.{writeLocalDate, writeLocalTime}

  val startOfDay: LocalTime = LocalTime.MIDNIGHT
  val endOfDay: LocalTime = startOfDay.minusSeconds(1)

  def entireDay(label: String, dates: Vector[LocalDate], global: Boolean): Vector[Blacklist] = {
    dates.map(d => entireDay(label, d, global))
  }

  def entireDay(label: String, date: LocalDate, global: Boolean): Blacklist = {
    Blacklist(label, date, startOfDay, endOfDay, global)
  }

  def partialDay(label: String, localDateTime: LocalDateTime, hourPadding: Int, global: Boolean): Blacklist = {
    val date = localDateTime.toLocalDate
    val start = localDateTime.toLocalTime
    val end = start.plusHours(hourPadding)

    Blacklist(label, date, start, end, global)
  }

  implicit val writes: Writes[Blacklist] = Json.writes[Blacklist]
}

object BlacklistProtocol {
  implicit val reads: Reads[BlacklistProtocol] = Json.reads[BlacklistProtocol]
}