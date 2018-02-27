package services

import models.BlacklistDb
import play.api.libs.json.{JsObject, JsValue}

import scala.concurrent.Future

object BlacklistService {

  private def shortLegalHolidayLabel(year: String) = s"NRW $year"

  private def legalHolidayLabel(year: String, holiday: String) = s"${shortLegalHolidayLabel(year)} - $holiday"

  private def uri(year: String) = s"http://feiertage.jarmedia.de/api/?jahr=$year&nur_land=NW"

  def fetchLegalHolidays(year: String): Future[List[BlacklistDb]] = {
    import play.api.libs.ws.ning._
    import utils.LwmDateTime._

    import scala.concurrent.ExecutionContext.Implicits.global

    for {
      sslClient <- Future.successful(NingWSClient())
      _ = println(sslClient.config.isAcceptAnyCertificate)
      response <- sslClient.url(uri(year)).get
      json = response.json.asOpt[JsObject].fold(Seq.empty[(String, JsValue)])(_.fields)
      blacklistDbs = json.foldLeft(List.empty[BlacklistDb]) {
        case (list, (key, value)) => value.\("datum")
          .asOpt[String]
          .map(_.localDate)
          .fold(list)(date => list.+:(BlacklistDb.entireDay(legalHolidayLabel(year, key), date, global = true)))
      }
      _ <- Future.successful(sslClient.close())
    } yield blacklistDbs
  }
}