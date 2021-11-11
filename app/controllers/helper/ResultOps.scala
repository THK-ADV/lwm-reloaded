package controllers.helper

import play.api.Logger
import play.api.libs.json.Json.JsValueWrapper
import play.api.libs.json.{JsValue, Json, Writes}
import play.api.mvc.{BaseController, Result}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

trait ResultOps {
  self: BaseController =>

  private val errorLogger = Logger("httpResponse")

  protected def errorJson(throwable: Throwable): JsValue = Json.obj(
    "message" -> throwable.getMessage,
    "trace" -> throwable.getStackTrace.map(_.toString)
  )

  private def msgJson(msg: String): JsValue = Json.obj("message" -> msg)

  protected def preconditionFailed(message: String): Result = PreconditionFailed(Json.obj("status" -> "KO", "message" -> message))

  protected def badRequest(throwable: Throwable): Result = BadRequest(errorJson(throwable))

  protected def ok[A](entity: A)(implicit writes: Writes[A]): Result = Ok(Json.toJson(entity))

  protected def ok(fields: (String, JsValueWrapper)*): Result = Ok(Json.obj(fields: _*))

  protected def notFound(element: String): Result = NotFound(msgJson(s"No such element for $element"))

  private def recoverBadRequest(): PartialFunction[Throwable, Result] = {
    case NonFatal(e) =>
      errorLogger.error(e.getMessage)
      badRequest(e)
  }

  implicit class SeqResult[A](val future: Future[Seq[A]]) {
    def jsonResult(implicit writes: Writes[A], executor: ExecutionContext): Future[Result] = future
      .map(a => ok(a))
      .recover(recoverBadRequest())
  }

  implicit class OptionResult[A](val future: Future[Option[A]]) {
    def jsonResult(idForMessage: String)(implicit writes: Writes[A], executor: ExecutionContext): Future[Result] = future
      .map(maybeA => maybeA.fold(notFound(idForMessage))(a => ok(a)))
      .recover(recoverBadRequest())
  }

  implicit class CreatedResult[A](val future: Future[A]) {
    def created(implicit writes: Writes[A], executor: ExecutionContext): Future[Result] = future
      .map(a => Created(Json.toJson(a)))
      .recover(recoverBadRequest())

    def jsonResult(implicit writes: Writes[A], executor: ExecutionContext): Future[Result] = future
      .map(a => ok(a))
      .recover(recoverBadRequest())

    def jsonResult(f: A => Result)(implicit executor: ExecutionContext): Future[Result] = future
      .map(f)
      .recover(recoverBadRequest())
  }

  implicit class PartialResult[A](val future: Future[(Seq[A], Seq[A], List[Throwable])]) {
    def jsonResult(implicit writes: Writes[A], executor: ExecutionContext): Future[Result] = future
      .map { res =>
        val (attempted, created, throwable) = res
        val status = if (created.isEmpty) "KO" else if (throwable.isEmpty) "OK" else "Partial OK"

        ok(
          "status" -> status,
          "attempted" -> Json.toJson(attempted),
          "created" -> Json.toJson(created),
          "failed" -> Json.toJson(throwable.map(_.getLocalizedMessage))
        )
      }.recover(recoverBadRequest())
  }

}