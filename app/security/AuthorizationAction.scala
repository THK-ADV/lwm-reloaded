package security

import auth.{OAuthAuthorization, UserToken}
import controllers.helper.RequestOps
import javax.inject.{Inject, Singleton}
import play.api.libs.json.Json
import play.api.mvc.Results.Unauthorized
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

@Singleton
case class AuthorizationAction @Inject()(auth: OAuthAuthorization)(val parser: BodyParsers.Default)(implicit val executionContext: ExecutionContext)
  extends ActionBuilder[IdRequest, AnyContent] with RequestOps {

  override def invokeBlock[A](request: Request[A], block: IdRequest[A] => Future[Result]): Future[Result] = auth.authorized(request).flatMap {
    case token: UserToken => block(IdRequest(request.withUserToken(token), token.systemId))
  }.recover {
    case NonFatal(e) => Unauthorized(Json.obj(
      "status" -> "KO",
      "message" -> e.getLocalizedMessage
    ))
  }
}
