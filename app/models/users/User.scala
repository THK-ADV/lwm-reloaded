package models.users

import models.{UniqueEntity, UriGenerator}
import play.api.libs.json.{JsValue, Json, Writes}

trait User extends UniqueEntity {
  def systemId: String

  def lastname: String

  def firstname: String

  def email: String
}

object User extends UriGenerator[User] {
  override def base: String = "users"

  lazy val employeeType = "employee"

  lazy val lecturerType = "lecturer"

  lazy val studentType = "student"
}