package keycloakapi

import play.api.libs.json.{JsError, JsResult, JsSuccess, JsValue}
import play.api.libs.ws.WSClient

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class KeycloakApiService @Inject() (
    private val config: KeycloakApiConfig,
    private val ws: WSClient,
    private implicit val ctx: ExecutionContext
) {
  private def authUrl =
    s"${config.keycloakBaseUrl}/auth/realms/master/protocol/openid-connect/token"

  private def usersUrl =
    s"${config.keycloakBaseUrl}/auth/admin/realms/${config.realm}/users"

  private def authPayload = Map(
    "grant_type" -> "client_credentials",
    "client_secret" -> config.adminCliClientSecret,
    "client_id" -> config.adminCliClientId
  )

  def parseBearerToken(js: JsValue): Future[String] =
    (js \ "access_token").validate[String] match {
      case JsSuccess(value, _) =>
        Future.successful(s"bearer $value")
      case JsError(errors) =>
        Future.failed(new Throwable(errors.mkString(",")))
    }

  def parseKeycloakUser(js: JsValue): JsResult[List[KeycloakUser]] =
    js.validate[List[KeycloakUser]]

  def authenticate: Future[String] =
    ws
      .url(authUrl)
      .withHttpHeaders(
        "Accept" -> "application/json",
        "Content-Type" -> "application/x-www-form-urlencoded"
      )
      .post(authPayload)
      .flatMap(r => parseBearerToken(r.json))

  def fetchUser[A](user: A)(username: A => String) = {
    def go(token: String) =
      ws.url(usersUrl)
        .withHttpHeaders(
          "Authorization" -> token,
          "Content-Type" -> "application/json"
        )
        .withQueryStringParameters("username" -> username(user))
        .get()
        .map(r => (user, takeFirst(parseKeycloakUser(r.json))))

    authenticate.flatMap(go)
  }

  private def takeFirst(
      users: JsResult[List[KeycloakUser]]
  ): JsResult[KeycloakUser] =
    users.flatMap {
      case h :: Nil => JsSuccess(h)
      case xs       => JsError(s"expect exactly one user, but found: $xs")
    }
}
