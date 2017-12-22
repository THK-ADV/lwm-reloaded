package models

import play.api.libs.json.{Json, Reads}

case class Login(username: String, password: String)

object Login {
  implicit val reads: Reads[Login] = Json.reads[Login]
}
