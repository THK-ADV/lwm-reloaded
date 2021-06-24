package keycloakapi

import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._

case class KeycloakUser(
    firstname: String,
    lastname: String,
    email: String,
    systemId: String,
    degreeAbbrev: Option[String],
    registrationId: Option[String]
)

object KeycloakUser {
  def firstStringOf(path: JsPath): Reads[String] =
    path.read[List[String]].map(_.head)

  def firstStringOrNullOf(path: JsPath): Reads[Option[String]] =
    path.readNullable[List[String]].map(_.flatMap(_.headOption))

  implicit val reads: Reads[KeycloakUser] = (
      (JsPath \ "firstName").read[String] and
      (JsPath \ "lastName").read[String] and
      (JsPath \ "email").read[String] and
      firstStringOf(JsPath \ "attributes" \ "systemId") and
      firstStringOrNullOf(JsPath \ "attributes" \ "degreeAbbrev") and
      firstStringOrNullOf(JsPath \ "attributes" \ "registrationId")
  )(KeycloakUser.apply _)
}
