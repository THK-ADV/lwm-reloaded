package controllers.helper

import play.api.libs.json.{JsError, JsValue, Reads}
import play.api.mvc.{AnyContent, Request}

import scala.util.{Failure, Success, Try}

trait JsonParser {
  final protected def parseJson[R](request: Request[AnyContent])(implicit reads: Reads[R]): Try[R] = unwrap(request).flatMap(js => validate(js)(reads))

  final protected def parseJsonArray[R](request: Request[AnyContent])(implicit reads: Reads[List[R]]): Try[List[R]] = unwrap(request).flatMap(js => validate(js)(reads))

  private def validate[A](json: JsValue)(implicit reads: Reads[A]): Try[A] = json.validate[A].fold[Try[A]](
    errors => Failure(new Throwable(JsError.toJson(errors).toString)),
    success => Success(success)
  )

  private def unwrap(request: Request[AnyContent]): Try[JsValue] = request.body.asJson match {
    case Some(json) => Success(json)
    case None => Failure(new Throwable("no json body"))
  }
}
