package models.semester

import java.util.UUID

import controllers.crud.JsonSerialisation
import models.{UriGenerator, UniqueEntity}
import org.joda.time.DateTime
import play.api.libs.json.{Format, Json, Reads, Writes}

case class Blacklist(label: String, dates: Set[DateTime], id: UUID) extends UniqueEntity {

  import Blacklist.dateOrd

  override def equals(that: scala.Any): Boolean = that match {
    case Blacklist(l, e, i) => l == label && dates.toVector.sorted.zip(e.toVector.sorted).forall(d => d._1.isEqual(d._2)) && id == i
    case _ => false
  }
}

case class BlacklistProtocol(label: String, dates: Set[DateTime]) {
  import Blacklist.dateOrd

  override def equals(that: scala.Any): Boolean = that match {
    case BlacklistProtocol(l, e) => l == label && dates.toVector.sorted.zip(e.toVector.sorted).forall(d => d._1.isEqual(d._2))
    case _ => false
  }
}

object Blacklist extends UriGenerator[Blacklist] with JsonSerialisation[BlacklistProtocol, Blacklist] {

  implicit val dateOrd: Ordering[DateTime] = new Ordering[DateTime] {
    override def compare(x: DateTime, y: DateTime): Int = x.compareTo(y)
  }

  lazy val empty = Blacklist("empty", Set.empty[DateTime], Blacklist.randomUUID)

  override def base: String = "blacklists"

  override implicit def reads: Reads[BlacklistProtocol] = Json.reads[BlacklistProtocol]

  override implicit def writes: Writes[Blacklist] = Json.writes[Blacklist]

  implicit def format: Format[Blacklist] = Json.format[Blacklist]

  implicit def protocolFormat: Format[BlacklistProtocol] = Json.format[BlacklistProtocol]
}
