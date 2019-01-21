package models

import models.User

sealed trait BuddyResult {
  override def toString: String = getClass.getSimpleName
}

case class Allowed(buddy: User) extends BuddyResult

case class Almost(buddy: User) extends BuddyResult

case class Denied(buddy: User) extends BuddyResult

case class NotExisting(buddy: String) extends BuddyResult

