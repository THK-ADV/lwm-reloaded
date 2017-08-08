package controllers.helper

import java.util.UUID

import controllers.{ContentTyped, SessionChecking}
import dao.AuthorityDao
import models.{Permissions, SesamePermission}
import play.api.libs.json.JsValue
import play.api.mvc.{Action, AnyContent, Request, Result}
import utils.LwmActions2.{ContentTypedAction, SecureAction, SecureContentTypedAction}

import scala.concurrent.Future

/**
  * `SecureControllerContext` provides an algebra for separately and indirectly composing
  * a `SecureAction` with the `Permission`s required to run that `SecureAction`.
  *
  * Each controller has specialised restrictions for their respective
  * CRUD operations. This means that they must somehow be "deferred to"
  * the generalised specification of these restrictions.
  *
  */
trait SecureControllerContext2 {
  self: Secured2 with SessionChecking with ContentTyped =>

  //to be specialized
  protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case _ => PartialSecureBlock(Permissions.prime)
  }

  //to be specialized
  protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case _ => PartialSecureBlock(Permissions.prime)
  }

  sealed trait Rule

  trait SecureContext {

    def action(block: Request[AnyContent] => Result): Action[AnyContent] = apply[AnyContent](
      restricted = (opt, perms) => SecureAction((opt, perms))(block),
      simple = Action(block)
    )

    def apply[A](restricted: (Option[UUID], SesamePermission) => Action[A], simple: => Action[A]) = this match {
      case SecureBlock(id, permission) => restricted(Some(UUID.fromString(id)), permission)
      case PartialSecureBlock(permission) => restricted(None, permission)
      case NonSecureBlock => simple()
    }

    def contentTypedAction(block: Request[JsValue] => Result): Action[JsValue] = apply[JsValue](
      restricted = (opt, perms) => SecureContentTypedAction((opt, perms))(block),
      simple = ContentTypedAction(block)
    )

    def asyncAction(block: Request[AnyContent] => Future[Result]): Action[AnyContent] = apply[AnyContent](
      restricted = (opt, perms) => SecureAction.async((opt, perms))(block),
      simple = Action.async(block)
    )

    def asyncContentTypedAction(block: Request[JsValue] => Future[Result]): Action[JsValue] = apply[JsValue](
      restricted = (opt, perms) => SecureContentTypedAction.async((opt, perms))(block),
      simple = ContentTypedAction.async(block)
    )
  }

  case class SecureBlock(restrictionRef: String, permission: SesamePermission) extends SecureContext

  case class PartialSecureBlock(permission: SesamePermission) extends SecureContext

  case object Create extends Rule

  case object Delete extends Rule

  case object GetAll extends Rule

  case object Get extends Rule

  case object Update extends Rule

  case object NonSecureBlock extends SecureContext

}

trait Secured2 {
  implicit def authorityDao: AuthorityDao
}