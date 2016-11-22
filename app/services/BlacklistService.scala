package services

import models.{Blacklist, TimetableDateEntry}
import org.joda.time.DateTime

import scala.concurrent.Future

sealed trait BlacklistDate {
  def date: DateTime

  def matching(timetableDateEntry: TimetableDateEntry) = this match {
    case EntireDate(entire) =>
      timetableDateEntry.date.isEqual(entire.toLocalDate)
    case PartialDate(partial) =>
      val startDate = timetableDateEntry.date.toDateTime(timetableDateEntry.start)
      val endDate = timetableDateEntry.date.toDateTime(timetableDateEntry.end)
      startDate.isEqual(partial) || partial.isAfter(startDate) && partial.isBefore(endDate)
  }
}

case class EntireDate(date: DateTime) extends BlacklistDate

case class PartialDate(date: DateTime) extends BlacklistDate

object BlacklistService {

  def legalHolidayLabel(year: String) = s"NRW Feiertage $year"

  def apiUri(year: String) = s"http://feiertage.jarmedia.de/api/?jahr=$year&nur_land=NW"
}

trait BlacklistServiceLike {

  def filterBy(entries: Vector[TimetableDateEntry], blacklists: Set[DateTime]): Vector[TimetableDateEntry]

  def fetchByYear(year: String): Future[Blacklist]
}

class BlacklistService extends BlacklistServiceLike {

  override def filterBy(entries: Vector[TimetableDateEntry], blacklists: Set[DateTime]): Vector[TimetableDateEntry] = (lift _ andThen filterNot(entries))(blacklists)

  override def fetchByYear(year: String): Future[Blacklist] = {
    import services.BlacklistService._
    import play.api.libs.ws.ning._
    import scala.concurrent.ExecutionContext.Implicits.global

    for {
      sslClient <- Future.successful(NingWSClient())
      response <- sslClient.url(apiUri(year)).get
      pattern = "(\"datum\":\".*?\")".r
      json = response.json.toString
      dates = (pattern findAllMatchIn json map (matches => DateTime.parse(matches.matched.split(":")(1).replace("\"", "")))).toSet
      _ <- Future.successful(sslClient.close())
    } yield Blacklist(legalHolidayLabel(year), dates)
  }

  private def lift(blacklist: Set[DateTime]): Set[BlacklistDate] = blacklist map {
    case entire if entire.getHourOfDay == 0 => EntireDate(entire)
    case partial => PartialDate(partial)
  }

  private def filterNot(entries: Vector[TimetableDateEntry])(blacklists: Set[BlacklistDate]) = entries filterNot (entry => blacklists exists(_.matching(entry)))
}