package utils

import akka.util.Timeout
import play.api.libs.json.{JsValue, Json, Reads}
import play.api.mvc.Result
import play.api.test.Helpers

import scala.concurrent.Future

object StreamHandler {

  lazy val emptyJson = Set(Json.parse("{}"))

  private def normalise(s: String): Set[JsValue] = s match {
    case empty if s.isEmpty => emptyJson
    case valid if s.contains("}{") =>
      (s.split("\\}\\{") map { s =>
        if (s endsWith "}") Json.parse("{" + s)
        else if (s startsWith "{") Json.parse(s + "}")
        else Json.parse(s"{$s}")
      }).toSet
    case _ => Set(Json.parse(s))
  }

  def contentFromStream(result: Future[Result])(implicit timeout: Timeout): Set[JsValue] = {
    normalise(Helpers.contentAsString(result))
  }

  def typedContentFromStream[A](result: Future[Result])(implicit timeout: Timeout, reads: Reads[A]): Set[A] = {
    normalise(Helpers.contentAsString(result)) map (js => Json.fromJson[A](js)) filter (_.isSuccess) map (_.get)
  }
}

