package security

import dao.AuthorityDao
import javax.inject.{Inject, Singleton}
import play.api.libs.json.Json
import play.api.mvc.Results.{Unauthorized, InternalServerError}
import play.api.mvc.{ActionRefiner, Result}

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class AuthorityAction @Inject()(authorityDao: AuthorityDao)(implicit val executionContext: ExecutionContext)
  extends ActionRefiner[IdRequest, AuthRequest] {

  override protected def refine[A](request: IdRequest[A]): Future[Either[Result, AuthRequest[A]]] = authorityDao.authoritiesFor(request.systemId).map { authorities =>
    Either.cond(authorities.nonEmpty, AuthRequest(request, authorities), Unauthorized(Json.obj(
      "status" -> "KO",
      "message" -> s"No authority found for ${request.systemId}"
    )))
  }.recover {
    case t => Left(InternalServerError(Json.obj(
      "status" -> "KO",
      "message" -> t.getLocalizedMessage
    )))
  }
}
