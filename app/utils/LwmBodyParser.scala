package utils

import play.api.http.LazyHttpErrorHandler
import play.api.http.Status._
import play.api.libs.json.JsValue
import play.api.mvc.{BodyParser, BodyParsers, RequestHeader, Result}

import scala.concurrent.Future

object LwmBodyParser extends BodyParsers {

  def parseWith(mimeType: LwmMimeType): BodyParser[JsValue] = parse.when (
    _.contentType.exists(m => m.equalsIgnoreCase(mimeType)),
    parse.tolerantJson(parse.DefaultMaxTextLength),
    createBadResult(s"Expecting ${mimeType.value} body", UNSUPPORTED_MEDIA_TYPE)
  )

  private def createBadResult(msg: String, statusCode: Int = BAD_REQUEST): RequestHeader => Future[Result] = { request =>
    LazyHttpErrorHandler.onClientError(request, statusCode, msg)
  }
}