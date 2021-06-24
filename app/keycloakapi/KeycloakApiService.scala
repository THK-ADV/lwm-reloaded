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

  private def parseBearerToken(js: JsValue): Future[String] =
    (js \ "access_token").validate[String] match {
      case JsSuccess(value, _) =>
        Future.successful(s"Bearer $value")
      case JsError(errors) =>
        Future.failed(new Throwable(errors.mkString(",")))
    }

  def authenticate: Future[String] =
    ws
      .url(authUrl)
      .withHttpHeaders(
        "Accept" -> "application/json",
        "Content-Type" -> "application/x-www-form-urlencoded"
      )
      .post(authPayload)
      .flatMap(r => parseBearerToken(r.json))

  def fetchUsers[A](users: Seq[A])(username: A => String) =
    for {
      token <- authenticate
      users <- Future.sequence(users.map(u => user(token, u, username(u))))
    } yield users

  private def user[A](token: String, user: A, username: String) =
    ws.url(usersUrl)
      .withHttpHeaders(
        "Authorization" -> token,
        "Content-Type" -> "application/json"
      )
      .withQueryStringParameters("username" -> username)
      .get()
      .map(r =>
        takeFirst(r.json.validate[List[KeycloakUser]]).map(ku => (user, ku))
      )

  private def takeFirst(
      users: JsResult[List[KeycloakUser]]
  ): JsResult[KeycloakUser] =
    users.flatMap {
      case h :: Nil => JsSuccess(h)
      case xs       => JsError(s"expect only one user, but found: $xs")
    }
}
