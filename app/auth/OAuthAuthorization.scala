package auth

import play.api.mvc.Request

import scala.concurrent.Future

trait OAuthAuthorization {
  import auth.OAuthAuthorization._

  def authorized[R](request: Request[R]): Future[VerifiedToken]

  def bearerToken[_](request: Request[_]): Option[String] = request.headers.get(AuthorizationHeader)
    .map(_.split(" "))
    .filter(_.length == 2)
    .filter(_.head.equalsIgnoreCase(BearerPrefix))
    .map(_.last)
    .filter(_.nonEmpty)
}

object OAuthAuthorization {
  val AuthorizationHeader = "Authorization"
  val BearerPrefix = "Bearer"
}