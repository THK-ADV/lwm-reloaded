package services.blacklist

import models.BlacklistDb
import play.api.libs.json.{JsObject, JsValue}

import scala.concurrent.Future

trait BlacklistService {
  def fetchLegalHolidays(year: Int): Future[List[BlacklistDb]]
}

final class BlacklistServiceImpl extends BlacklistService {

  private def shortLegalHolidayLabel(year: Int) = s"NRW $year"

  private def legalHolidayLabel(year: Int, holiday: String) = s"${shortLegalHolidayLabel(year)} - $holiday"

  private def uri(year: Int) = s"http://feiertage.jarmedia.de/api/?jahr=$year&nur_land=NW"

//  def fetchLegalHolidays(year: Int): Future[List[BlacklistDb]] = {
//    import play.api.libs.ws.ning._
//    import utils.LwmDateTime._
//
//    import scala.concurrent.ExecutionContext.Implicits.global
//
//    for {
//      sslClient <- Future.successful(NingWSClient())
//      response <- sslClient.url(uri(year)).get
//      json = response.json.asOpt[JsObject].fold(Seq.empty[(String, JsValue)])(_.fields)
//      blacklistDbs = json.foldLeft(List.empty[BlacklistDb]) {
//        case (list, (key, value)) => value.\("datum")
//          .asOpt[String]
//          .map(_.localDate)
//          .fold(list)(date => list.+:(BlacklistDb.entireDay(legalHolidayLabel(year, key), date, global = true)))
//      }
//      _ <- Future.successful(sslClient.close())
//    } yield blacklistDbs
//  }

  def fetchLegalHolidays(year: Int): Future[List[BlacklistDb]] = Future.successful(List.empty) // TODO
}