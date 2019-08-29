package security

import java.util.UUID
import java.util.concurrent.Executors

import dao.RoleDao
import javax.inject.Inject
import models.{Authority, LWMRole}
import play.api.libs.json.Json
import play.api.mvc.Results.Forbidden
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}

trait SecurityActionChain {
  protected def authorizationAction: AuthorizationAction

  protected def authorityAction: AuthorityAction

  protected def roleDao: RoleDao

  def secured[R <: LWMRole](restricted: Option[UUID], required: List[R])(block: Request[AnyContent] => Result): Action[AnyContent]

  def securedAsync[R <: LWMRole](restricted: Option[UUID], required: List[R])(block: Request[AnyContent] => Future[Result]): Action[AnyContent]
}

final class SecurityActionChainImpl @Inject()(val authorizationAction: AuthorizationAction, val authorityAction: AuthorityAction, val roleDao: RoleDao) extends SecurityActionChain {

  private implicit val actionExecutionContext: ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool)

  private def securedAction(predicate: Seq[Authority] => Future[Boolean]) = {
    authorizationAction andThen authorityAction andThen allowed(predicate)
  }

  def secured[R <: LWMRole](restricted: Option[UUID], required: List[R])(block: Request[AnyContent] => Result): Action[AnyContent] = {
    securedAction(authorities => roleDao.isAuthorized(restricted, required)(authorities)).apply(block)
  }

  def securedAsync[R <: LWMRole](restricted: Option[UUID], required: List[R])(block: Request[AnyContent] => Future[Result]): Action[AnyContent] = {
    securedAction(authorities => roleDao.isAuthorized(restricted, required)(authorities)).async(block)
  }

  private def allowed(predicate: Seq[Authority] => Future[Boolean]) = new ActionFilter[AuthRequest] {
    override protected def filter[A](request: AuthRequest[A]): Future[Option[Result]] = predicate(request.authorities).map { allowed =>
      if (allowed) None else Some(Forbidden(Json.obj(
        "status" -> "KO",
        "message" -> "Insufficient permissions for given action"
      )))
    }

    override protected def executionContext: ExecutionContext = actionExecutionContext
  }
}