package database

import java.sql.{Date, Time, Timestamp}
import java.util.UUID

import models.{Blacklist, BlacklistProtocol, UniqueDbEntity}
import org.joda.time.{DateTime, LocalDate}
import slick.jdbc.PostgresProfile.api._
import utils.date.DateTimeOps._

class BlacklistTable(tag: Tag) extends Table[BlacklistDb](tag, "BLACKLIST") with UniqueTable with LabelTable with DateStartEndTable {
  def global = column[Boolean]("GLOBAL")

  override def * = (label, date, start, end, global, lastModified, invalidated, id) <> ((BlacklistDb.apply _).tupled, BlacklistDb.unapply)
}

case class BlacklistDb(label: String, date: Date, start: Time, end: Time, global: Boolean, lastModified: Timestamp = DateTime.now.timestamp, invalidated: Option[Timestamp] = None, id: UUID = UUID.randomUUID) extends UniqueDbEntity {
  override def toUniqueEntity = Blacklist(label, date.localDate, start.localTime, end.localTime, global, id)

  override def equals(that: scala.Any) = that match {
    case BlacklistDb(l, d, s, e, g, _, _, i) =>
      l == label &&
        d.localDate.isEqual(date.localDate) && s
        .localTime.isEqual(start.localTime) &&
        e.localTime.isEqual(end.localTime) &&
        g == global &&
        i == id
    case _ => false
  }
}

object BlacklistDb {

  import models.Blacklist.{endOfDay, startOfDay}

  def from(protocol: BlacklistProtocol, existingId: Option[UUID]): BlacklistDb = {
    BlacklistDb(protocol.label, protocol.date.sqlDate, protocol.start.sqlTime, protocol.end.sqlTime, protocol.global, id = existingId.getOrElse(UUID.randomUUID))
  }

  def entireDay(label: String, date: LocalDate, global: Boolean): BlacklistDb = entireDay(label, date.sqlDate, global)

  def entireDay(label: String, date: Date, global: Boolean): BlacklistDb = BlacklistDb(label, date, startOfDay.sqlTime, endOfDay.sqlTime, global)
}