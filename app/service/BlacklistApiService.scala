package service

import database.BlacklistDb
import javax.inject.Inject
import org.joda.time.LocalDate
import play.api.libs.json.{JsObject, JsValue}
import utils.date.DateTimeFormatterPattern

import scala.concurrent.{ExecutionContext, Future}

trait BlacklistApiService {
  def fetchLegalHolidays(year: Int)(implicit executor: ExecutionContext): Future[List[BlacklistDb]]

  protected def shortLegalHolidayLabel(year: Int) = s"NRW $year"

  protected def legalHolidayLabel(year: Int, holiday: String) = s"${shortLegalHolidayLabel(year)} - $holiday"

  protected def uri(year: Int) = s"http://feiertage.jarmedia.de/api/?jahr=$year&nur_land=NW"
}

final class BlacklistApiServiceImpl @Inject()(ws: Webservice) extends BlacklistApiService with DateTimeFormatterPattern {

  def fetchLegalHolidays(year: Int)(implicit executor: ExecutionContext): Future[List[BlacklistDb]] = {
    ws.get(uri(year))(j => parse(j, year))
  }

  def parse(json: JsValue, year: Int): List[BlacklistDb] = {
    val blacklists = for {
      (key, value) <- json.asOpt[JsObject].fold(Seq.empty[(String, JsValue)])(_.fields) if (value \ "datum").isDefined
      dateString = (value \ "datum").as[String]
      date = LocalDate.parse(dateString, dateFormatter)
    } yield BlacklistDb.entireDay(legalHolidayLabel(year, key), date, global = true)

    blacklists.toList
  }
}