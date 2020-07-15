package controllers.helper

import java.util.UUID

import org.joda.time.{DateTime, LocalDate, LocalTime}
import play.api.libs.json._
import play.api.mvc.{AnyContent, Request}

import scala.util.{Failure, Success, Try}

trait JsonParser {

  implicit def listReads[R](implicit r: Reads[R]): Reads[List[R]] = Reads.list[R](r)

  implicit def listWrites[W](implicit w: Writes[W]): Writes[List[W]] = Writes.list[W](w)

  implicit def seqWrites[W](implicit w: Writes[W]): Writes[Seq[W]] = Writes.seq[W](w)

  final protected def parseJson[R](request: Request[AnyContent])(implicit reads: Reads[R]): Try[R] = unwrap(request).flatMap(js => validate(js)(reads))

  final protected def parseJsonArray[R](request: Request[AnyContent])(implicit reads: Reads[List[R]]): Try[List[R]] = unwrap(request).flatMap(js => validate(js)(reads))

  protected def validate[A](json: JsValue)(implicit reads: Reads[A]): Try[A] = asTry(json.validate[A])

  protected def unwrap(request: Request[AnyContent]): Try[JsValue] = request.body.asJson match {
    case Some(json) => Success(json)
    case None => Failure(new Throwable("json body is required"))
  }

  protected def asTry[A](r: JsResult[A]): Try[A] = r.fold[Try[A]](
    errors => Failure(new Throwable(JsError.toJson(errors).toString)),
    success => Success(success)
  )

  protected def string[A <: JsReadable](r: A): JsResult[String] = r.validate[String]

  protected def int[A <: JsReadable](r: A): JsResult[Int] = r.validate[Int]

  protected def bool[A <: JsReadable](r: A): JsResult[Boolean] = r.validate[Boolean]

  protected def uuid[A <: JsReadable](r: A): JsResult[UUID] = string(r).map(UUID.fromString)

  protected def dateTime[A <: JsReadable](r: A)(implicit reads: Reads[DateTime]): JsResult[DateTime] = r.validate[DateTime]

  protected def localTime[A <: JsReadable](r: A)(implicit reads: Reads[LocalTime]): JsResult[LocalTime] = r.validate[LocalTime]

  protected def localDate[A <: JsReadable](r: A)(implicit reads: Reads[LocalDate]): JsResult[LocalDate] = r.validate[LocalDate]
}
