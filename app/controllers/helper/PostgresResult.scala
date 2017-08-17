package controllers.helper

import java.util.UUID

import play.api.libs.json.{Json, Writes}
import play.api.mvc.{Controller, Result}

import scala.concurrent.Future
import scala.util.control.NonFatal

trait PostgresResult { self: Controller =>

  import scala.concurrent.ExecutionContext.Implicits.global

  protected def preconditionFailed(message: String): Result = PreconditionFailed(Json.obj("status" -> "KO", "message" -> message))
  protected def internalServerError(throwable: Throwable): Result = internalServerError(throwable.getMessage)
  protected def internalServerError(message: String): Result = InternalServerError(Json.obj("status" -> "KO", "message" -> message))
  protected def ok[A](entity: A)(implicit writes: Writes[A]): Result = Ok(Json.toJson(entity))
  protected def notFound(element: String): Result = NotFound(Json.obj("status" -> "KO", "message" -> s"No such element for $element"))

  implicit class SequenceResult[A](val future: Future[Seq[A]]) {
    def jsonResult(implicit writes: Writes[A]): Future[Result] = future.map(a => ok(a)).recover {
      case NonFatal(e) => internalServerError(e)
    }
  }

  implicit class OptionResult[A](val future: Future[Option[A]]) {
    def jsonResult(idForMessage: String)(implicit writes: Writes[A]): Future[Result] = future.map { maybeA =>
      maybeA.fold(notFound(idForMessage))(ok)
    }.recover {
      case NonFatal(e) => internalServerError(e)
    }

    def jsonResult(idOfEntity: UUID)(implicit writes: Writes[A]): Future[Result] = future.map { maybeA =>
      maybeA.fold(internalServerError(s"cant update or delete $idOfEntity"))(ok)
    }.recover {
      case NonFatal(e) => internalServerError(e)
    }
  }

  implicit class CreatedResult[A](val future: Future[A]) {
    def jsonResult(implicit writes: Writes[A]): Future[Result] = future.map(a => Created(Json.toJson(a))).recover {
      case NonFatal(e) => internalServerError(e)
    }

    def jsonResult(f: A => Result): Future[Result] = future.map(f).recover {
      case NonFatal(e) => internalServerError(e)
    }
  }
}