package models

import java.sql.{Date, Time, Timestamp}
import java.util.UUID

import controllers.JsonSerialisation
import models.LwmDateTime.dateTimeOrd
import org.joda.time.{DateTime, LocalDate, LocalDateTime, LocalTime}
import play.api.libs.functional.syntax._
import play.api.libs.json._
import utils.Ops.JsPathX
import models.LwmDateTime._

case class SesameBlacklist(label: String, dates: Set[DateTime], invalidated: Option[DateTime] = None, id: UUID = SesameBlacklist.randomUUID) extends UniqueEntity {
  override def equals(that: scala.Any): Boolean = that match {
    case SesameBlacklist(l, e, _, i) => l == label && dates.toVector.sorted.zip(e.toVector.sorted).forall(d => d._1.isEqual(d._2)) && id == i
    case _ => false
  }
}

case class SesameBlacklistProtocol(label: String, dates: Set[String]) {
  override def equals(that: scala.Any): Boolean = that match {
    case SesameBlacklistProtocol(l, e) =>
      l == label &&
        dates.map(LwmDateTime.toDateTime).toVector.sorted.zip(e.map(LwmDateTime.toDateTime).toVector.sorted).forall(d => d._1.isEqual(d._2))
    case _ => false
  }
}

object SesameBlacklist extends UriGenerator[SesameBlacklist] with JsonSerialisation[SesameBlacklistProtocol, SesameBlacklist, SesameBlacklist] {

  lazy val empty = SesameBlacklist("empty", Set.empty[DateTime])

  override def base: String = "blacklists"

  override implicit def reads: Reads[SesameBlacklistProtocol] = Json.reads[SesameBlacklistProtocol]

  override implicit def writes: Writes[SesameBlacklist] = (
    (JsPath \ "label").write[String] and
      (JsPath \ "dates").writeSet[DateTime](LwmDateTime.writes) and
      (JsPath \ "invalidated").writeNullable[DateTime] and
      (JsPath \ "id").write[UUID]
    )(unlift(SesameBlacklist.unapply))

  override implicit def writesAtom: Writes[SesameBlacklist] = writes
}

// POSTGRES

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

object PostgresBlacklist extends JsonSerialisation[PostgresBlacklistProtocol, PostgresBlacklist, PostgresBlacklist] {

  val startOfDay: LocalTime = LocalTime.MIDNIGHT
  val endOfDay: LocalTime = startOfDay.minusSeconds(1)

  def entireDay(label: String, date: LocalDate, global: Boolean): PostgresBlacklist = {
    PostgresBlacklist(label, date, startOfDay, endOfDay, global)
  }

  def entireDay(label: String, dates: Vector[LocalDate], global: Boolean): Vector[PostgresBlacklist] = {
    dates.map(d => entireDay(label, d, global))
  }

  def partialDay(label: String, localDateTime: LocalDateTime, hourPadding: Int, global: Boolean): PostgresBlacklist = {
    val date = localDateTime.toLocalDate
    val start = localDateTime.toLocalTime
    val end = start.plusHours(hourPadding)

    PostgresBlacklist(label, date, start, end, global)
  }

  override implicit def reads = Json.reads[PostgresBlacklistProtocol]

  override implicit def writes = Json.writes[PostgresBlacklist]

  override implicit def writesAtom = writes
}

object BlacklistDb {
  import models.PostgresBlacklist.{endOfDay, startOfDay}

  def from(protocol: PostgresBlacklistProtocol, existingId: Option[UUID]): BlacklistDb = {
    BlacklistDb(protocol.label, protocol.date.sqlDateFromPattern, protocol.start.sqlTimeFromPattern, protocol.end.sqlTimeFromPattern, protocol.global, id = existingId.getOrElse(UUID.randomUUID))
  }

  def entireDay(label: String, date: LocalDate, global: Boolean): BlacklistDb = entireDay(label, date.sqlDate, global)

  def entireDay(label: String, date: Date, global: Boolean): BlacklistDb = BlacklistDb(label, date, startOfDay.sqlTime, endOfDay.sqlTime, global)
}