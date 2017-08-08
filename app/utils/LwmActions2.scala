package utils

import java.util.UUID
import java.util.concurrent.Executors

import controllers.SessionController
import dao.AuthorityDao
import models.{PostgresAuthority, SesamePermission}
import play.api.libs.json.{JsValue, Json}
import play.api.mvc.Results.{InternalServerError, Unauthorized}
import play.api.mvc._
import services.SessionHandlingService

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

object LwmActions2 {

  implicit val actionExecutionContext: ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

  final private def securedAction(predicate: Seq[PostgresAuthority] => Future[Boolean])(implicit authorityDao: AuthorityDao, sessionService: SessionHandlingService) = {
    Allowed(sessionService) andThen Authorized(authorityDao) andThen Permitted(predicate)
  }

  case class AuthRequest[A](private val unwrapped: Request[A], authorities: Seq[PostgresAuthority]) extends WrappedRequest[A](unwrapped)

  case class IdRequest[A](private val unwrapped: Request[A], userId: String) extends WrappedRequest[A](unwrapped)

  case class Allowed(sessionService: SessionHandlingService) extends ActionBuilder[IdRequest] {

    override def invokeBlock[A](request: Request[A], block: (IdRequest[A]) => Future[Result]): Future[Result] = {
      (request.session.get(SessionController.sessionId), request.session.get(SessionController.userId)) match {
        case (Some(sessionId), Some(userId)) =>
          sessionService.isValid(UUID.fromString(sessionId)).flatMap { valid =>
            if (valid)
              block(IdRequest(request, userId))
            else
              Future.successful {
                Unauthorized(Json.obj(
                  "status" -> "KO",
                  "message" -> "Session invalid or non-existent"
                ))
              }
          }
        case (None, _) =>
          Future.successful {
            Unauthorized(Json.obj(
              "status" -> "KO",
              "message" -> "No session-id found in session"
            ))
          }
        case (_, None) =>
          Future.successful {
            Unauthorized(Json.obj(
              "status" -> "KO",
              "message" -> "No user-id found in session"
            ))
          }
      }
    }
  }

  case class Authorized(authorityDao: AuthorityDao) extends ActionFunction[IdRequest, AuthRequest] {

    override def invokeBlock[A](request: IdRequest[A], block: (AuthRequest[A]) => Future[Result]): Future[Result] = {
      def f = block compose (AuthRequest.apply[A] _).curried(request)

      authorityDao.authoritiesFor(UUID.fromString(request.userId)).flatMap { authorities =>
        if (authorities.nonEmpty)
          f(authorities)
        else
          Future.successful {
            Unauthorized(Json.obj(
              "status" -> "KO",
              "message" -> s"No authority found for ${request.userId}"
            ))
          }
      }.recover {
        case NonFatal(e) =>
          InternalServerError(Json.obj(
            "status" -> "KO",
            "message" -> s"${e.getMessage}"
          ))
      }
    }
  }

  case class Permitted(predicate: Seq[PostgresAuthority] => Future[Boolean]) extends ActionFilter[AuthRequest] {

    override protected def filter[A](request: AuthRequest[A]): Future[Option[Result]] = predicate(request.authorities).map { allowed =>
      if (allowed)
        None
      else
        Some(Unauthorized(Json.obj(
          "status" -> "KO",
          "message" -> "Insufficient permissions for given action"
        )))
    }.recover {
      case NonFatal(e) =>
        Some(InternalServerError(Json.obj(
          "status" -> "KO",
          "message" -> e.getMessage
        )))
    }
  }

  object ContentTypedAction {

    def apply(block: Request[JsValue] => Result)(implicit mimeType: LwmMimeType): Action[JsValue] = Action(LwmBodyParser.parseWith(mimeType))(block)

    def async(block: Request[JsValue] => Future[Result])(implicit mimeType: LwmMimeType): Action[JsValue] = Action.async(LwmBodyParser.parseWith(mimeType))(block)

  }

  object SecureAction {

    def apply(ps: (Option[UUID], SesamePermission))(block: Request[AnyContent] => Result)(implicit authorityDao: AuthorityDao, sessionService: SessionHandlingService) = {
      securedAction(authorities => authorityDao.checkAuthority(ps)(authorities))(authorityDao, sessionService)(block)
    }

    def async(ps: (Option[UUID], SesamePermission))(block: Request[AnyContent] => Future[Result])(implicit authorityDao: AuthorityDao, sessionService: SessionHandlingService) = {
      securedAction(authorities => authorityDao.checkAuthority(ps)(authorities)).async(block)
    }
  }

  object SecureContentTypedAction {

    def apply(ps: (Option[UUID], SesamePermission))(block: Request[JsValue] => Result)(implicit mimeType: LwmMimeType, authorityDao: AuthorityDao, sessionService: SessionHandlingService) = {
      securedAction(authorities => authorityDao.checkAuthority(ps)(authorities))(authorityDao, sessionService)(LwmBodyParser.parseWith(mimeType))(block)
    }

    def async(ps: (Option[UUID], SesamePermission))(block: Request[JsValue] => Future[Result])(implicit mimeType: LwmMimeType, authorityDao: AuthorityDao, sessionService: SessionHandlingService) = {
      securedAction(authorities => authorityDao.checkAuthority(ps)(authorities)).async(LwmBodyParser.parseWith(mimeType))(block)
    }
  }
}
