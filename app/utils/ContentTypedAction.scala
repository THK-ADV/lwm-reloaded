package utils

import play.api.libs.json.JsValue
import play.api.mvc._

import services.RoleServiceLike

import scala.concurrent.Future

object LWMActions {

  object ContentTypedAction {

    def apply(block: Request[JsValue] => Result)(implicit mimeType: LWMMimeType): Action[JsValue] = Action(LWMBodyParser.parseWith(mimeType))(block)

    def async(block: Request[JsValue] => Future[Result])(implicit mimeType: LWMMimeType): Action[JsValue] = Action.async(LWMBodyParser.parseWith(mimeType))(block)

  }

  object SecureAction {

    def apply[A]()(predicate: Set[A] => Boolean)(block: Request[AnyContent] => Result)(implicit roleService: RoleServiceLike[A]) = {
      securedAction[A](predicate)(roleService)(block)
    }

    def apply[A](permissions: Set[A])(block: Request[AnyContent] => Result)(implicit roleService: RoleServiceLike[A]) = {
      securedAction[A](userPerms => roleService.checkWith(permissions)(userPerms))(roleService)(block)
    }

    def async[A]()(predicate: Set[A] => Boolean)(block: Request[AnyContent] => Future[Result])(implicit roleService: RoleServiceLike[A]) = {
      securedAction[A](predicate)(roleService).async(block)
    }

    def async[A](permissions: Set[A])(block: Request[AnyContent] => Future[Result])(implicit roleService: RoleServiceLike[A]) = {
      securedAction[A](userPerms => roleService.checkWith(permissions)(userPerms))(roleService).async(block)
    }

  }

  object SecuredContentTypedAction {

    def apply[A]()(predicate: Set[A] => Boolean)(block: Request[JsValue] => Result)(implicit mimeType: LWMMimeType, roleService: RoleServiceLike[A]) = {
      securedAction[A](predicate)(roleService)(LWMBodyParser.parseWith(mimeType))(block)
    }

    def apply[A](permissions: Set[A])(block: Request[JsValue] => Result)(implicit mimeType: LWMMimeType, roleService: RoleServiceLike[A]) = {
      securedAction[A](userPerms => roleService.checkWith(permissions)(userPerms))(roleService)(LWMBodyParser.parseWith(mimeType))(block)
    }

    def async[A]()(predicate: Set[A] => Boolean)(block: Request[JsValue] => Future[Result])(implicit mimeType: LWMMimeType, roleService: RoleServiceLike[A]) = {
      securedAction[A](predicate)(roleService).async(LWMBodyParser.parseWith(mimeType))(block)
    }

    def async[A](permissions: Set[A])(block: Request[JsValue] => Future[Result])(implicit mimeType: LWMMimeType, roleService: RoleServiceLike[A]) = {
      securedAction[A](userPerms => roleService.checkWith(permissions)(userPerms))(roleService).async(LWMBodyParser.parseWith(mimeType))(block)
    }

  }

  final private def securedAction[R](predicate: Set[R] => Boolean)(implicit roleService: RoleServiceLike[R]) = Allowed[R](roleService) andThen Permitted[R](predicate)
}


case class AuthRequest[A, R](private val unwrapped: Request[A], userPermissions: Set[R]) extends WrappedRequest[A](unwrapped)


case class Permitted[R](predicate: Set[R] => Boolean) extends ActionFilter[({type L[A] = AuthRequest[A, R]})#L] {

  override protected def filter[A](request: AuthRequest[A, R]): Future[Option[Result]] = Future.successful {
    if(predicate(request.userPermissions))
      None
    else
      Some(Results.Unauthorized("Insufficient permissions for given action"))
  }
}


case class Allowed[R](roleService: RoleServiceLike[R]) extends ActionBuilder[({type L[A] = AuthRequest[A, R]})#L] {

  override def invokeBlock[A](request: Request[A], block: (AuthRequest[A, R]) => Future[Result]): Future[Result] = {
    request.session.get(Security.username) match {
      case Some(username) => (block compose (AuthRequest.apply[A, R] _).curried(request) compose roleService.permissionsFor)(username)
      case _ => (block compose (AuthRequest.apply[A, R] _).curried(request))(Set.empty[R])
    }
  }


}


