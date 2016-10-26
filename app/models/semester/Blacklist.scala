package models.semester

import java.util.UUID

import controllers.crud.JsonSerialisation
import models.{UriGenerator, UniqueEntity}
import org.joda.time.DateTime
import play.api.libs.json.{Format, Json, Reads, Writes}

case class Blacklist(label: String, dates: Set[DateTime], invalidated: Option[DateTime] = None, id: UUID = Blacklist.randomUUID) extends UniqueEntity {

  import models.semester.Blacklist.dateOrd

  override def equals(that: scala.Any): Boolean = that match {
    case Blacklist(l, e, _, i) => l == label && dates.toVector.sorted.zip(e.toVector.sorted).forall(d => d._1.isEqual(d._2)) && id == i
    case _ => false
  }
}

case class BlacklistProtocol(label: String, dates: Set[DateTime]) {
  import models.semester.Blacklist.dateOrd

  override def equals(that: scala.Any): Boolean = that match {
    case BlacklistProtocol(l, e) => l == label && dates.toVector.sorted.zip(e.toVector.sorted).forall(d => d._1.isEqual(d._2))
    case _ => false
  }
}

object Blacklist extends UriGenerator[Blacklist] with JsonSerialisation[BlacklistProtocol, Blacklist, Blacklist] {

  implicit val dateOrd: Ordering[DateTime] = new Ordering[DateTime] {
    override def compare(x: DateTime, y: DateTime): Int = x.compareTo(y)
  }

  lazy val empty = Blacklist("empty", Set.empty[DateTime])

  override def base: String = "blacklists"

  override implicit def reads: Reads[BlacklistProtocol] = Json.reads[BlacklistProtocol]

  override implicit def writes: Writes[Blacklist] = Json.writes[Blacklist]

  override implicit def writesAtom: Writes[Blacklist] = writes
}
