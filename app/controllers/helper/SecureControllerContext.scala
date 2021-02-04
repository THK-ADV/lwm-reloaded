package controllers.helper

import play.api.mvc._
import security.LWMRole.{BasicRole, CourseRelatedRole}
import security.{LWMRole, SecurityActionChain}

import java.util.UUID
import scala.concurrent.Future

trait SecureControllerContext {
  self: BaseController =>

  final protected def forbiddenAction(): PartialFunction[Rule, SecureContext] = {
    case _ => PartialSecureBlock(List(LWMRole.God))
  }

  protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext]

  protected def contextFrom: PartialFunction[Rule, SecureContext]

  protected def securedAction: SecurityActionChain

  sealed trait SecureContext {

    def action(block: Request[AnyContent] => Result): Action[AnyContent] = apply[AnyContent](
      restricted = (id, roles) => securedAction.secured(id, roles)(block),
      simple = Action(block)
    )

    def asyncAction(block: Request[AnyContent] => Future[Result]): Action[AnyContent] = apply[AnyContent](
      restricted = (id, roles) => securedAction.securedAsync(id, roles)(block),
      simple = Action.async(block)
    )

    def apply[A](restricted: (Option[UUID], List[LWMRole]) => Action[A], simple: => Action[A]) = this match {
      case SecureBlock(id, roles) => restricted(Some(UUID.fromString(id)), roles)
      case PartialSecureBlock(roles) => restricted(None, roles)
      case NonSecureBlock => simple()
    }
  }

  case class SecureBlock(restrictionRef: String, roles: List[CourseRelatedRole]) extends SecureContext

  case class PartialSecureBlock(roles: List[BasicRole]) extends SecureContext

  case object NonSecureBlock extends SecureContext

  sealed trait Rule

  case object Create extends Rule

  case object Delete extends Rule

  case object GetAll extends Rule

  case object Get extends Rule

  case object Update extends Rule
}