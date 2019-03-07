package controllers.helper

import play.api.libs.json.{Json, Writes}
import play.api.mvc.{BaseController, Result}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

trait ResultOps {
  self: BaseController =>

  protected def preconditionFailed(message: String): Result = PreconditionFailed(Json.obj("status" -> "KO", "message" -> message))

  protected def internalServerError(throwable: Throwable): Result = internalServerError(throwable.getMessage)

  protected def internalServerError(message: String): Result = InternalServerError(Json.obj("status" -> "KO", "message" -> message))

  protected def ok[A](entity: A)(implicit writes: Writes[A]): Result = Ok(Json.toJson(entity))

  protected def delete[A](entity: A)(implicit writes: Writes[A]): Result = Ok(Json.obj("deleted" -> Json.toJson(entity)))

  protected def update[A](entity: A)(implicit writes: Writes[A]): Result = Ok(Json.obj("updated" -> Json.toJson(entity)))

  protected def notFound(element: String): Result = NotFound(Json.obj("status" -> "KO", "message" -> s"No such element for $element"))

  implicit class SequenceResult[A](val future: Future[Seq[A]]) {
    def jsonResult(implicit writes: Writes[A], executor: ExecutionContext): Future[Result] = future.map(a => ok(a)).recover {
      case NonFatal(e) => internalServerError(e)
    }

    def deleted(implicit writes: Writes[A], executor: ExecutionContext): Future[Result] = future.map(a => delete(a)).recover {
      case NonFatal(e) => internalServerError(e)
    }
  }

  implicit class OptionResult[A](val future: Future[Option[A]]) {
    def jsonResult(idForMessage: String)(implicit writes: Writes[A], executor: ExecutionContext): Future[Result] = future.map { maybeA =>
      maybeA.fold(notFound(idForMessage))(ok)
    }.recover {
      case NonFatal(e) => internalServerError(e)
    }
  }

  implicit class CreatedResult[A](val future: Future[A]) {
    def created(implicit writes: Writes[A], executor: ExecutionContext): Future[Result] = future.map(a => Created(Json.toJson(a))).recover {
      case NonFatal(e) => internalServerError(e)
    }

    def deleted(implicit writes: Writes[A], executor: ExecutionContext): Future[Result] = future.map(a => delete(a)).recover {
      case NonFatal(e) => internalServerError(e)
    }

    def updated(implicit writes: Writes[A], executor: ExecutionContext): Future[Result] = future.map(a => update(a)).recover {
      case NonFatal(e) => internalServerError(e)
    }

    def jsonResult(f: A => Result)(implicit executor: ExecutionContext): Future[Result] = future.map(f).recover {
      case NonFatal(e) => internalServerError(e)
    }
  }

  implicit class PartialResult[A](val future: Future[(Seq[A], Seq[A], List[Throwable])]) {
    def jsonResult(implicit writes: Writes[A], executor: ExecutionContext): Future[Result] = future.map { res =>
      val (attempted, created, throwable) = res
      val status = if (created.isEmpty) "KO" else if (throwable.isEmpty) "OK" else "Partial OK"

      Ok(Json.obj(
        "status" -> status,
        "attempted" -> Json.toJson(attempted),
        "created" -> Json.toJson(created),
        "failed" -> Json.toJson(throwable.map(_.getLocalizedMessage))
      ))
    }.recover {
      case NonFatal(e) => internalServerError(e)
    }
  }

}