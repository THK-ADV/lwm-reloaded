package utils.date

import org.joda.time.{DateTime, LocalDate, LocalDateTime, LocalTime}
import play.api.libs.json._

import scala.util.{Failure, Success, Try}

object DateTimeJsonFormatter extends DateTimeFormatterPattern {

  implicit val writeDateTime: Writes[DateTime] = Writes(a => JsString(a.toString(dateTimeFormatter)))

  implicit val writeLocalDateTime: Writes[LocalDateTime] = Writes(a => JsString(a.toString(dateTimeFormatter)))

  implicit val writeLocalDate: Writes[LocalDate] = Writes(a => JsString(a.toString(dateFormatter)))

  implicit val writeLocalTime: Writes[LocalTime] = Writes(a => JsString(a.toString(timeFormatter)))

  implicit val readLocalDate: Reads[LocalDate] = Reads { js =>
    val triedDate = jsStringValue(js).flatMap(s => Try(dateFormatter.parseLocalDate(s)))
    jsResult(triedDate)
  }

  implicit val readLocalTime: Reads[LocalTime] = Reads { js =>
    val triedDate = jsStringValue(js).flatMap(s => Try(timeFormatter.parseLocalTime(s)))
    jsResult(triedDate)
  }


  implicit val readLocalDateTime: Reads[DateTime] = Reads { js =>
    val triedDate = jsStringValue(js).flatMap(s => Try(dateTimeFormatter.parseDateTime(s)))
    jsResult(triedDate)
  }

  protected final def jsResult[A](a: Try[A]): JsResult[A] = a match {
    case Success(s) => JsSuccess(s)
    case Failure(e) => JsError(e.getLocalizedMessage)
  }

  protected final def jsStringValue(js: JsValue): Try[String] = Try(js.as[JsString].value)
}
