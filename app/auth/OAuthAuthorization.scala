package auth

import play.api.mvc.Request

import scala.concurrent.Future

trait OAuthAuthorization {
  import auth.OAuthAuthorization._

  def authorized[R](request: Request[R]): Future[VerifiedToken]

  def bearerToken[_](request: Request[_]): Option[String] = request.headers.get(AuthorizationHeader)
    .filter(_.contains(BearerPrefix))
    .map(_.stripPrefix(BearerPrefix))
    .filter(_.nonEmpty)
}

object OAuthAuthorization {
  val AuthorizationHeader = "Authorization"
  val BearerPrefix = "Bearer "
}