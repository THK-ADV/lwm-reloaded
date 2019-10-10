package controllers.helper

import play.api.libs.json.{JsError, JsValue, Reads, Writes}
import play.api.mvc.{AnyContent, Request}

import scala.util.{Failure, Success, Try}

trait JsonParser {

  implicit def listReads[R](implicit r: Reads[R]): Reads[List[R]] = Reads.list[R](r)

  implicit def listWrites[W](implicit w: Writes[W]): Writes[List[W]] = Writes.list[W](w)

  implicit def seqWrites[W](implicit w: Writes[W]): Writes[Seq[W]] = Writes.seq[W](w)

  final protected def parseJson[R](request: Request[AnyContent])(implicit reads: Reads[R]): Try[R] = unwrap(request).flatMap(js => validate(js)(reads))

  final protected def parseJsonArray[R](request: Request[AnyContent])(implicit reads: Reads[List[R]]): Try[List[R]] = unwrap(request).flatMap(js => validate(js)(reads))

  def validate[A](json: JsValue)(implicit reads: Reads[A]): Try[A] = json.validate[A].fold[Try[A]](
    errors => Failure(new Throwable(JsError.toJson(errors).toString)),
    success => Success(success)
  )

  private def unwrap(request: Request[AnyContent]): Try[JsValue] = request.body.asJson match {
    case Some(json) => Success(json)
    case None => Failure(new Throwable("json body is required"))
  }
}
