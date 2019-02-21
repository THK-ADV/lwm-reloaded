package security

import controllers.helper.RequestOps
import dao.{AuthorityDao, UserDao}
import javax.inject.{Inject, Singleton}
import play.api.libs.json.Json
import play.api.mvc.Results.InternalServerError
import play.api.mvc.{ActionRefiner, Result}

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class AuthorityAction @Inject()(authorityDao: AuthorityDao, userDao: UserDao)(implicit val executionContext: ExecutionContext)
  extends ActionRefiner[IdRequest, AuthRequest] with RequestOps {

  override protected def refine[A](request: IdRequest[A]): Future[Either[Result, AuthRequest[A]]] = authorityDao.authoritiesFor(request.systemId).flatMap { authorities =>
    if (authorities.nonEmpty)
      Future.successful(Right(AuthRequest(request, authorities)))
    else
      request.unwrapped.userToken match {
        case Some(token) =>
          for {
            result <- userDao.createOrUpdateWithBasicAuthority(token.systemId, token.lastName, token.firstName, token.email, token.status, token.registrationId, token.degreeAbbrev)
            user = result.entity
            auths <- authorityDao.authoritiesFor(user.systemId)
          } yield Right(AuthRequest(request, auths))
        case None =>
          Future.successful(Left(error(s"no ${RequestOps.UserToken} found in request")))
      }
  }.recover {
    case t => Left(error(t.getLocalizedMessage))
  }

  private def error(message: String): Result = InternalServerError(Json.obj(
    "status" -> "KO",
    "message" -> message
  ))
}
