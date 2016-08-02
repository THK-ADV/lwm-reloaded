package utils

import java.util.UUID
import java.util.concurrent.Executors

import controllers.SessionController
import models.security.{Authority, Permission}
import play.api.libs.json.{JsValue, Json}
import play.api.mvc.Results._
import play.api.mvc._
import services.{RoleServiceLike, SessionHandlingService}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

object LwmActions {

  implicit val actionExecutionContext: ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

  final private def securedAction(predicate: Seq[Authority] => Try[Boolean])(implicit roleService: RoleServiceLike, sessionService: SessionHandlingService) = {
    Allowed(sessionService) andThen Authorized(roleService) andThen Permitted(predicate)
  }

  object ContentTypedAction {

    def apply(block: Request[JsValue] => Result)(implicit mimeType: LwmMimeType): Action[JsValue] = Action(LwmBodyParser.parseWith(mimeType))(block)

    def async(block: Request[JsValue] => Future[Result])(implicit mimeType: LwmMimeType): Action[JsValue] = Action.async(LwmBodyParser.parseWith(mimeType))(block)

  }

  object SecureAction {

    def apply(ps: (Option[UUID], Permission))(block: Request[AnyContent] => Result)(implicit roleService: RoleServiceLike, sessionService: SessionHandlingService) = {
      securedAction(authorities => roleService.checkAuthority(ps)(authorities:_*))(roleService, sessionService)(block)
    }

    def async(ps: (Option[UUID], Permission))(block: Request[AnyContent] => Future[Result])(implicit roleService: RoleServiceLike, sessionService: SessionHandlingService) = {
      securedAction(authorities => roleService.checkAuthority(ps)(authorities:_*)).async(block)
    }
  }

  object SecureContentTypedAction {

    def apply(ps: (Option[UUID], Permission))(block: Request[JsValue] => Result)(implicit mimeType: LwmMimeType, roleService: RoleServiceLike, sessionService: SessionHandlingService) = {
      securedAction(authorities => roleService.checkAuthority(ps)(authorities:_*))(roleService, sessionService)(LwmBodyParser.parseWith(mimeType))(block)
    }

    def async(ps: (Option[UUID], Permission))(block: Request[JsValue] => Future[Result])(implicit mimeType: LwmMimeType, roleService: RoleServiceLike, sessionService: SessionHandlingService) = {
      securedAction(authorities => roleService.checkAuthority(ps)(authorities:_*)).async(LwmBodyParser.parseWith(mimeType))(block)
    }
  }
}

case class AuthRequest[A](private val unwrapped: Request[A], authorities: Seq[Authority]) extends WrappedRequest[A](unwrapped)

case class IdRequest[A](private val unwrapped: Request[A], userId: String) extends WrappedRequest[A](unwrapped)

case class Allowed(sessionService: SessionHandlingService) extends ActionBuilder[IdRequest] {

  import utils.LwmActions.actionExecutionContext

  override def invokeBlock[A](request: Request[A], block: (IdRequest[A]) => Future[Result]): Future[Result] = {
    (request.session.get(SessionController.sessionId), request.session.get(SessionController.userId)) match {
      case (Some(sessionId), Some(userId)) =>
        sessionService.isValid(UUID.fromString(sessionId)) flatMap { valid =>
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

case class Authorized(roleService: RoleServiceLike) extends ActionFunction[IdRequest, AuthRequest] {

  override def invokeBlock[A](request: IdRequest[A], block: (AuthRequest[A]) => Future[Result]): Future[Result] = {
    def f = block compose (AuthRequest.apply[A] _).curried(request)

    roleService.authorities(UUID.fromString(request.userId)) match {
      case Success(authorities) =>
        if (authorities.nonEmpty)
          f(authorities.toSeq)
        else
          Future.successful {
            Unauthorized(Json.obj(
              "status" -> "KO",
              "message" -> s"No authority found for ${request.userId}"
            ))
          }
      case Failure(e) =>
        Future.successful {
          InternalServerError(Json.obj(
            "status" -> "KO",
            "message" -> s"${e.getMessage}"
          ))
        }
    }
  }
}

case class Permitted(predicate: Seq[Authority] => Try[Boolean]) extends ActionFilter[AuthRequest] {

  override protected def filter[A](request: AuthRequest[A]): Future[Option[Result]] = Future.successful {
    predicate(request.authorities) match {
      case Success(allowed) if allowed => None
      case Success(_) =>
        Some(Unauthorized(Json.obj(
          "status" -> "KO",
          "message" -> "Insufficient permissions for given action"
        )))
      case Failure(e) =>
        Some(InternalServerError(Json.obj(
          "status" -> "KO",
          "message" -> e.getMessage
        )))
    }
  }
}
