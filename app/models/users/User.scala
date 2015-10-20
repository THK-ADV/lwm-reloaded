package models.users

import models.{UniqueEntity, UriGenerator}

trait User extends UniqueEntity {
  def systemId: String

  def lastname: String

  def firstname: String

  def email: String

}

object User extends UriGenerator[User] {
  override def base: String = "users"
}