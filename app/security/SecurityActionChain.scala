package security

import java.util.UUID
import java.util.concurrent.Executors

import controllers.helper.RequestOps
import dao.{AuthorityDao, UserDao}
import javax.inject.{Inject, Singleton}
import models.{Authority, LWMRole}
import play.api.libs.json.Json
import play.api.mvc.Results.{Forbidden, Unauthorized}
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}

@Singleton
final class SecurityActionChain @Inject()(authorizationAction: AuthorizationAction, authorityAction: AuthorityAction, authorityDao: AuthorityDao, userDao: UserDao) extends RequestOps {

  private implicit val actionExecutionContext: ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool)

  private def securedAction(predicate: Seq[Authority] => Future[Boolean]) = {
    authorizationAction andThen authorityAction andThen allowed(predicate)
  }

  def secured[R <: LWMRole](ps: (Option[UUID], List[R]))(block: Request[AnyContent] => Result) = {
    securedAction(authorities => authorityDao.checkAuthority(ps)(authorities)).apply(block)
  }

  def securedAsync[R <: LWMRole](ps: (Option[UUID], List[R]))(block: Request[AnyContent] => Future[Result]) = {
    securedAction(authorities => authorityDao.checkAuthority(ps)(authorities)).async(block)
  }

//  private def authorized = new ActionRefiner[IdRequest, AuthRequest] {
//    override protected def refine[A](request: IdRequest[A]): Future[Either[Result, AuthRequest[A]]] = authorityDao.authoritiesFor(request.systemId).map { authorities =>
//      Either.cond(authorities.nonEmpty, AuthRequest(request, authorities), Unauthorized(Json.obj(
//        "status" -> "KO",
//        "message" -> s"No authority found for ${request.systemId}"
//      )))
//    }
//
//    //    override protected def refine[A](request: IdRequest[A]): Future[Either[Result, AuthRequest[A]]] = authorityDao.authoritiesFor(request.systemId).map {
//    //      case authorities =>
//    //        Right(AuthRequest(request, authorities))
//    //      case Nil =>
//    //        val token = request.userToken.get
//    //
//    //        ???
//    //    }
//
//    override protected def executionContext: ExecutionContext = actionExecutionContext
//  }

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