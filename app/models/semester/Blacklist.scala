package models.semester

import java.util.UUID

import controllers.crud.JsonSerialisation
import models.{LwmDateTime, UniqueEntity, UriGenerator}
import org.joda.time.DateTime
import play.api.libs.json._
import models.LwmDateTime.dateTimeOrd
import play.api.libs.functional.syntax._
import utils.Ops.JsPathX

case class Blacklist(label: String, dates: Set[DateTime], invalidated: Option[DateTime] = None, id: UUID = Blacklist.randomUUID) extends UniqueEntity {
  override def equals(that: scala.Any): Boolean = that match {
    case Blacklist(l, e, _, i) => l == label && dates.toVector.sorted.zip(e.toVector.sorted).forall(d => d._1.isEqual(d._2)) && id == i
    case _ => false
  }
}

case class BlacklistProtocol(label: String, dates: Set[String]) {
  override def equals(that: scala.Any): Boolean = that match {
    case BlacklistProtocol(l, e) =>
      l == label &&
        dates.map(LwmDateTime.toDateTime).toVector.sorted.zip(e.map(LwmDateTime.toDateTime).toVector.sorted).forall(d => d._1.isEqual(d._2))
    case _ => false
  }
}

object Blacklist extends UriGenerator[Blacklist] with JsonSerialisation[BlacklistProtocol, Blacklist, Blacklist] {

  lazy val empty = Blacklist("empty", Set.empty[DateTime])

  override def base: String = "blacklists"

  override implicit def reads: Reads[BlacklistProtocol] = Json.reads[BlacklistProtocol]

  override implicit def writes: Writes[Blacklist] = (
    (JsPath \ "label").write[String] and
      (JsPath \ "dates").writeSet[DateTime](LwmDateTime.writes) and
      (JsPath \ "invalidated").writeNullable[DateTime] and
      (JsPath \ "id").write[UUID]
    )(unlift(Blacklist.unapply))

  override implicit def writesAtom: Writes[Blacklist] = writes
}
