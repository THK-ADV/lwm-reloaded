package services.blacklist

import database.BlacklistDb
import javax.inject.Inject
import play.api.libs.json.{JsObject, JsValue}
import services.Webservice

import scala.concurrent.{ExecutionContext, Future}

trait BlacklistService {
  def fetchLegalHolidays(year: Int)(implicit executor: ExecutionContext): Future[List[BlacklistDb]]

  protected def shortLegalHolidayLabel(year: Int) = s"NRW $year"

  protected def legalHolidayLabel(year: Int, holiday: String) = s"${shortLegalHolidayLabel(year)} - $holiday"

  protected def uri(year: Int) = s"http://feiertage.jarmedia.de/api/?jahr=$year&nur_land=NW"
}

final class BlacklistServiceImpl @Inject()(ws: Webservice) extends BlacklistService {

  def fetchLegalHolidays(year: Int)(implicit executor: ExecutionContext): Future[List[BlacklistDb]] = {
    import utils.LwmDateTime.StringDateConverter

    ws.get(uri(year)) { json =>
      val blacklists = for {
        (key, value) <- json.asOpt[JsObject].fold(Seq.empty[(String, JsValue)])(_.fields) if (value \ "datum").isDefined
        date = (value \ "datum").as[String]
      } yield BlacklistDb.entireDay(legalHolidayLabel(year, key), date.localDate, global = true)

      blacklists.toList
    }
  }
}