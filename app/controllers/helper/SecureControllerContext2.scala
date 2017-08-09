package controllers.helper

import java.util.UUID

import controllers.{ContentTyped, SessionChecking}
import dao.AuthorityDao
import models.Role
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

  import models.Role._

  //to be specialized
  protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case _ => PartialSecureBlock(List(Admin))
  }

  //to be specialized
  protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case _ => PartialSecureBlock(List(Admin))
  }

  sealed trait Rule

  trait SecureContext {

    def action(block: Request[AnyContent] => Result): Action[AnyContent] = apply[AnyContent](
      restricted = (opt, role) => SecureAction((opt, role))(block),
      simple = Action(block)
    )

    def contentTypedAction(block: Request[JsValue] => Result): Action[JsValue] = apply[JsValue](
      restricted = (opt, role) => SecureContentTypedAction((opt, role))(block),
      simple = ContentTypedAction(block)
    )

    def apply[A](restricted: (Option[UUID], List[Role]) => Action[A], simple: => Action[A]) = this match {
      case SecureBlock(id, role) => restricted(Some(UUID.fromString(id)), role)
      case PartialSecureBlock(role) => restricted(None, role)
      case NonSecureBlock => simple()
    }

    def asyncAction(block: Request[AnyContent] => Future[Result]): Action[AnyContent] = apply[AnyContent](
      restricted = (opt, role) => SecureAction.async((opt, role))(block),
      simple = Action.async(block)
    )

    def asyncContentTypedAction(block: Request[JsValue] => Future[Result]): Action[JsValue] = apply[JsValue](
      restricted = (opt, role) => SecureContentTypedAction.async((opt, role))(block),
      simple = ContentTypedAction.async(block)
    )
  }

  case class SecureBlock(restrictionRef: String, roles: List[Role]) extends SecureContext

  case class PartialSecureBlock(roles: List[Role]) extends SecureContext

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