package models

import play.api.libs.json.{Json, Reads}

case class Login(username: String, password: String)

object Login {

  implicit def reads: Reads[Login] = Json.reads[Login]
}
