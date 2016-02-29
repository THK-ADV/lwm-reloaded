package utils

import java.util.UUID

import controllers.SessionController
import models.security.{Authority, Permission}
import play.api.libs.json.{JsValue, Json}
import play.api.mvc._
import services.RoleServiceLike

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

object LWMActions {

  final private def securedAction(predicate: Authority => Try[Boolean])(implicit roleService: RoleServiceLike) = Allowed(roleService) andThen Permitted(predicate)

  object ContentTypedAction {

    def apply(block: Request[JsValue] => Result)(implicit mimeType: LwmMimeType): Action[JsValue] = Action(LwmBodyParser.parseWith(mimeType))(block)

    def async(block: Request[JsValue] => Future[Result])(implicit mimeType: LwmMimeType): Action[JsValue] = Action.async(LwmBodyParser.parseWith(mimeType))(block)

  }

  object SecureAction {

    def apply()(predicate: Authority => Try[Boolean])(block: Request[AnyContent] => Result)(implicit roleService: RoleServiceLike) = {
      securedAction(predicate)(roleService)(block)
    }

    def apply(ps: (Option[UUID], Set[Permission]))(block: Request[AnyContent] => Result)(implicit roleService: RoleServiceLike) = {
      securedAction(userAuth =>
        roleService.checkWith(ps)(userAuth.refRoles))(roleService)(block)
    }

    def async()(predicate: Authority => Try[Boolean])(block: Request[AnyContent] => Future[Result])(implicit roleService: RoleServiceLike) = {
      securedAction(predicate)(roleService).async(block)
    }

    def async(ps: (Option[UUID], Set[Permission]))(block: Request[AnyContent] => Future[Result])(implicit roleService: RoleServiceLike) = {
      securedAction(userAuth =>
        roleService.checkWith(ps)(userAuth.refRoles))(roleService).async(block)
    }


  }

  object SecureContentTypedAction {

    def apply()(predicate: Authority => Try[Boolean])(block: Request[JsValue] => Result)(implicit mimeType: LwmMimeType, roleService: RoleServiceLike) = {
      securedAction(predicate)(roleService)(LwmBodyParser.parseWith(mimeType))(block)
    }

    def apply(ps: (Option[UUID], Set[Permission]))(block: Request[JsValue] => Result)(implicit mimeType: LwmMimeType, roleService: RoleServiceLike) = {
      securedAction(userAuth =>
        roleService.checkWith(ps)(userAuth.refRoles))(roleService)(LwmBodyParser.parseWith(mimeType))(block)
    }

    def async()(predicate: Authority => Try[Boolean])(block: Request[JsValue] => Future[Result])(implicit mimeType: LwmMimeType, roleService: RoleServiceLike) = {
      securedAction(predicate)(roleService).async(LwmBodyParser.parseWith(mimeType))(block)
    }

    def async(ps: (Option[UUID], Set[Permission]))(block: Request[JsValue] => Future[Result])(implicit mimeType: LwmMimeType, roleService: RoleServiceLike) = {
      securedAction(userAuth =>
        roleService.checkWith(ps)(userAuth.refRoles))(roleService).async(LwmBodyParser.parseWith(mimeType))(block)
    }

  }

}


case class AuthRequest[A](private val unwrapped: Request[A], authority: Authority) extends WrappedRequest[A](unwrapped)


case class Permitted(predicate: Authority => Try[Boolean]) extends ActionFilter[AuthRequest] {

  override protected def filter[A](request: AuthRequest[A]): Future[Option[Result]] = Future.successful {
    predicate(request.authority) match {
      case Success(allowed) if allowed => None
      case Success(_) =>
        Some(Results.Unauthorized(Json.obj(
          "status" -> "KO",
          "message" -> "Insufficient permissions for given action"
        )))
      case Failure(e) =>
        Some(Results.InternalServerError(Json.obj(
          "status" -> "KO",
          "message" -> e.getMessage
        )))
    }
  }
}


case class Allowed(roleService: RoleServiceLike) extends ActionBuilder[AuthRequest] {

  override def invokeBlock[A](request: Request[A], block: (AuthRequest[A]) => Future[Result]): Future[Result] = {
    def f = block compose (AuthRequest.apply[A] _).curried(request)

    request.session.get(SessionController.userId) match {
      case Some(id) =>
        roleService.authorityFor(id) match {
          case Success(optAuth) if optAuth.isDefined => f(optAuth.get)
          case Success(_) => Future.successful {
            Results.Unauthorized(Json.obj(
              "status" -> "KO",
              "message" -> s"No authority found for $id"
            ))
          }
          case Failure(e) => Future.successful {
            Results.InternalServerError(Json.obj(
              "status" -> "KO",
              "message" -> s"${e.getMessage}"
            ))
          }
        }
      case None => Future.successful {
        Results.Unauthorized(Json.obj(
          "status" -> "KO",
          "message" -> s"No user-id present in session"
        ))
      }
    }
  }
}


