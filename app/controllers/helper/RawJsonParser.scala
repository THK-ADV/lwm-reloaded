package controllers.helper

import java.util.UUID

import org.joda.time.{DateTime, LocalDate, LocalTime}
import play.api.libs.json.{JsArray, JsError, JsReadable, JsValue, Reads, Writes}
import play.api.mvc.{AnyContent, Request}

import scala.util.{Failure, Success, Try}

trait RawJsonParser {

  implicit def listReads[R](implicit r: Reads[R]): Reads[List[R]] = Reads.list[R](r)

  implicit def listWrites[W](implicit w: Writes[W]): Writes[List[W]] = Writes.list[W](w)

  implicit def seqWrites[W](implicit w: Writes[W]): Writes[Seq[W]] = Writes.seq[W](w)

  final protected def parseJson[R](request: Request[AnyContent])(implicit reads: Reads[R]): Try[R] = unwrap(request).flatMap(js => validate(js)(reads))

  final protected def parseJsonArray[R](request: Request[AnyContent])(implicit reads: Reads[List[R]]): Try[List[R]] = unwrap(request).flatMap(js => validate(js)(reads))

  protected def validate[A](json: JsValue)(implicit reads: Reads[A]): Try[A] = json.validate[A].fold[Try[A]](
    errors => Failure(new Throwable(JsError.toJson(errors).toString)),
    success => Success(success)
  )

  protected def unwrap(request: Request[AnyContent]): Try[JsValue] = request.body.asJson match {
    case Some(json) => Success(json)
    case None => Failure(new Throwable("json body is required"))
  }

  protected def string[A <: JsReadable](r: A): String = r.validate[String].get // TODO rewrite, because it is using outside of migration

  protected def int[A <: JsReadable](r: A): Int = r.validate[Int].get

  protected def bool[A <: JsReadable](r: A): Boolean = r.validate[Boolean].get

  protected def uuid[A <: JsReadable](r: A): UUID = UUID.fromString(string(r))

  protected def dateTime[A <: JsReadable](r: A)(implicit reads: Reads[DateTime]): DateTime = r.validate[DateTime].get

  protected def localTime[A <: JsReadable](r: A)(implicit reads: Reads[LocalTime]): LocalTime = r.validate[LocalTime].get

  protected def localDate[A <: JsReadable](r: A)(implicit reads: Reads[LocalDate]): LocalDate = r.validate[LocalDate].get

  protected def array[A <: JsReadable, B](r: A, f: JsValue => B): Set[B] = r.as[JsArray].value.map(f).toSet
}
