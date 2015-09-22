package utils

import controllers.SessionController
import models.security.{Authority, RefRole}
import play.api.libs.json.{JsValue, Json}
import play.api.mvc._
import services.RoleServiceLike

import scala.concurrent.Future

object LWMActions {

  object ContentTypedAction {

    def apply(block: Request[JsValue] => Result)(implicit mimeType: LwmMimeType): Action[JsValue] = Action(LwmBodyParser.parseWith(mimeType))(block)

    def async(block: Request[JsValue] => Future[Result])(implicit mimeType: LwmMimeType): Action[JsValue] = Action.async(LwmBodyParser.parseWith(mimeType))(block)

  }

  object SecureAction {

    def apply()(predicate: Authority => Boolean)(block: Request[AnyContent] => Result)(implicit roleService: RoleServiceLike) = {
      securedAction(predicate)(roleService)(block)
    }

    def apply(permissions: Set[RefRole])(block: Request[AnyContent] => Result)(implicit roleService: RoleServiceLike) = {
      securedAction(userAuth => roleService.checkWith(permissions)(userAuth.refRoles))(roleService)(block)
    }

    def async()(predicate: Authority => Boolean)(block: Request[AnyContent] => Future[Result])(implicit roleService: RoleServiceLike) = {
      securedAction(predicate)(roleService).async(block)
    }

    def async(permissions: Set[RefRole])(block: Request[AnyContent] => Future[Result])(implicit roleService: RoleServiceLike) = {
      securedAction(userAuth => roleService.checkWith(permissions)(userAuth.refRoles))(roleService).async(block)
    }

  }

  object SecureContentTypedAction {

    def apply()(predicate: Authority => Boolean)(block: Request[JsValue] => Result)(implicit mimeType: LwmMimeType, roleService: RoleServiceLike) = {
      securedAction(predicate)(roleService)(LwmBodyParser.parseWith(mimeType))(block)
    }

    def apply(permissions: Set[RefRole])(block: Request[JsValue] => Result)(implicit mimeType: LwmMimeType, roleService: RoleServiceLike) = {
      securedAction(userAuth => roleService.checkWith(permissions)(userAuth.refRoles))(roleService)(LwmBodyParser.parseWith(mimeType))(block)
    }

    def async()(predicate: Authority => Boolean)(block: Request[JsValue] => Future[Result])(implicit mimeType: LwmMimeType, roleService: RoleServiceLike) = {
      securedAction(predicate)(roleService).async(LwmBodyParser.parseWith(mimeType))(block)
    }

    def async(permissions: Set[RefRole])(block: Request[JsValue] => Future[Result])(implicit mimeType: LwmMimeType, roleService: RoleServiceLike) = {
      securedAction(userAuth => roleService.checkWith(permissions)(userAuth.refRoles))(roleService).async(LwmBodyParser.parseWith(mimeType))(block)
    }

  }

  final private def securedAction(predicate: Authority => Boolean)(implicit roleService: RoleServiceLike) = Allowed(roleService) andThen Permitted(predicate)
}


case class AuthRequest[A](private val unwrapped: Request[A], authority: Authority) extends WrappedRequest[A](unwrapped)


case class Permitted(predicate: Authority => Boolean) extends ActionFilter[AuthRequest] {

  override protected def filter[A](request: AuthRequest[A]): Future[Option[Result]] = Future.successful {
    if (predicate(request.authority))
      None
    else
      Some(Results.Unauthorized(Json.obj(
        "status" -> "KO",
        "message" -> "Insufficient permissions for given action"
      )))
  }
}


case class Allowed(roleService: RoleServiceLike) extends ActionBuilder[AuthRequest] {

  override def invokeBlock[A](request: Request[A], block: (AuthRequest[A]) => Future[Result]): Future[Result] = {
    def f = block compose (AuthRequest.apply[A] _).curried(request)

    (for {
      userId <- request.session.get(SessionController.userId)
      authority <- roleService.authorityFor(userId)
    } yield f(authority)) getOrElse f(Authority.empty)
  }


}


