package models

import java.sql.{Date, Time, Timestamp}
import java.util.UUID

import controllers.JsonSerialisation
import models.LwmDateTime.dateTimeOrd
import org.joda.time.{DateTime, LocalDate, LocalTime}
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

case class BlacklistDb(label: String, date: Date, start: Time, end: Time, global: Boolean, lastModified: Timestamp = DateTime.now.timestamp, invalidated: Option[Timestamp] = None, id: UUID = UUID.randomUUID) extends UniqueEntity {
  def toBlacklist = PostgresBlacklist(label, date.localDate, start.localTime, end.localTime, global, id)
}

object PostgresBlacklist extends JsonSerialisation[PostgresBlacklistProtocol, PostgresBlacklist, PostgresBlacklist] {

  override implicit def reads = Json.reads[PostgresBlacklistProtocol]

  override implicit def writes = Json.writes[PostgresBlacklist]

  override implicit def writesAtom = writes
}