package controllers.helper

import java.util.UUID

import models.{LWMRole, Role}
import play.api.mvc._
import utils.SecuredAction

import scala.concurrent.Future

trait SecureControllerContext {
  self: BaseController =>

  protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext]

  protected def contextFrom: PartialFunction[Rule, SecureContext]

  protected def securedAction: SecuredAction

  trait SecureContext {

    def action(block: Request[AnyContent] => Result): Action[AnyContent] = apply[AnyContent](
      restricted = (opt, role) => securedAction.secured((opt, role))(block),
      simple = Action(block)
    )

    def asyncAction(block: Request[AnyContent] => Future[Result]): Action[AnyContent] = apply[AnyContent](
      restricted = (opt, role) => securedAction.securedAsync((opt, role))(block),
      simple = Action.async(block)
    )

    def apply[A](restricted: (Option[UUID], List[LWMRole]) => Action[A], simple: => Action[A]) = this match {
      case SecureBlock(id, role) => restricted(Some(UUID.fromString(id)), role)
      case PartialSecureBlock(role) => restricted(None, role)
      case NonSecureBlock => simple()
    }
  }

  case class SecureBlock(restrictionRef: String, roles: List[LWMRole]) extends SecureContext

  case class PartialSecureBlock(roles: List[LWMRole]) extends SecureContext

  case object NonSecureBlock extends SecureContext

  sealed trait Rule

  case object Create extends Rule

  case object Delete extends Rule

  case object GetAll extends Rule

  case object Get extends Rule

  case object Update extends Rule

}