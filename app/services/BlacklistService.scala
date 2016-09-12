package services

import models.labwork.TimetableDateEntry
import models.semester.Blacklist
import org.joda.time.DateTime

import scala.concurrent.Future

object BlacklistService {

  def legalHolidayLabel(year: String) = s"NRW Feiertage $year"

  def apiUri(year: String) = s"http://feiertage.jarmedia.de/api/?jahr=$year&nur_land=NW"
}

trait BlacklistServiceLike {

  def applyBlacklist(entries: Vector[TimetableDateEntry], blacklists: Set[DateTime]): Vector[TimetableDateEntry]

  def fetchByYear(year: String): Future[Blacklist]
}

class BlacklistService extends BlacklistServiceLike {

  override def applyBlacklist(entries: Vector[TimetableDateEntry], blacklists: Set[DateTime]): Vector[TimetableDateEntry] = {
    def checkLocal(dateEntry: TimetableDateEntry, blacklist: DateTime): Boolean = {
      if (blacklist.getHourOfDay == 0)
        dateEntry.date.isEqual(blacklist.toLocalDate)
      else {
        val startDate = dateEntry.date.toDateTime(dateEntry.start)
        val endDate = dateEntry.date.toDateTime(dateEntry.end)
        startDate.isEqual(blacklist) || blacklist.isAfter(startDate) && blacklist.isBefore(endDate)
      }
    }

    entries.filterNot(e => blacklists.exists(bl => checkLocal(e, bl)))
  }

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
}