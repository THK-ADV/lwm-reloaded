package models

import java.sql.{Date, Time, Timestamp}
import java.util.UUID

import org.joda.time.{DateTime, LocalDate, LocalDateTime, LocalTime}
import play.api.libs.json._
import utils.LwmDateTime._

case class PostgresBlacklist(label: String, date: LocalDate, start: LocalTime, end: LocalTime, global: Boolean, id: UUID = UUID.randomUUID) extends UniqueEntity

case class PostgresBlacklistProtocol(label: String, date: String, start: String, end: String, global: Boolean)

case class BlacklistDb(label: String, date: Date, start: Time, end: Time, global: Boolean, lastModified: Timestamp = DateTime.now.timestamp, invalidated: Option[Timestamp] = None, id: UUID = UUID.randomUUID) extends UniqueDbEntity {
  override def toLwmModel = PostgresBlacklist(label, date.localDate, start.localTime, end.localTime, global, id)

  override def equals(that: scala.Any) = that match {
    case BlacklistDb(l, d, s, e, g, _, _, i) =>
      l == label && d.localDate.isEqual(date.localDate) && s.localTime.isEqual(start.localTime) && e.localTime.isEqual(end.localTime) && g == global && i == id
    case _ => false
  }
}

object PostgresBlacklist {

  val startOfDay: LocalTime = LocalTime.MIDNIGHT
  val endOfDay: LocalTime = startOfDay.minusSeconds(1)

  def entireDay(label: String, dates: Vector[LocalDate], global: Boolean): Vector[PostgresBlacklist] = {
    dates.map(d => entireDay(label, d, global))
  }

  def entireDay(label: String, date: LocalDate, global: Boolean): PostgresBlacklist = {
    PostgresBlacklist(label, date, startOfDay, endOfDay, global)
  }

  def partialDay(label: String, localDateTime: LocalDateTime, hourPadding: Int, global: Boolean): PostgresBlacklist = {
    val date = localDateTime.toLocalDate
    val start = localDateTime.toLocalTime
    val end = start.plusHours(hourPadding)

    PostgresBlacklist(label, date, start, end, global)
  }

  implicit val writes: Writes[PostgresBlacklist] = Json.writes[PostgresBlacklist]
}

object PostgresBlacklistProtocol {
  implicit val reads: Reads[PostgresBlacklistProtocol] = Json.reads[PostgresBlacklistProtocol]
}

object BlacklistDb {

  import models.PostgresBlacklist.{endOfDay, startOfDay}

  def from(protocol: PostgresBlacklistProtocol, existingId: Option[UUID]): BlacklistDb = {
    BlacklistDb(protocol.label, protocol.date.sqlDateFromPattern, protocol.start.sqlTimeFromPattern, protocol.end.sqlTimeFromPattern, protocol.global, id = existingId.getOrElse(UUID.randomUUID))
  }

  def entireDay(label: String, date: LocalDate, global: Boolean): BlacklistDb = entireDay(label, date.sqlDate, global)

  def entireDay(label: String, date: Date, global: Boolean): BlacklistDb = BlacklistDb(label, date, startOfDay.sqlTime, endOfDay.sqlTime, global)
}