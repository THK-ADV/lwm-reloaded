package models

import java.util.UUID

import org.joda.time.DateTime

sealed trait SessionValidation

case class Session(username: String, userId: UUID, id: UUID = UUID.randomUUID(), expirationDate: DateTime = DateTime.now().plusDays(1)) extends SessionValidation

case class ValidationFailure(s: String) extends SessionValidation

object Permissions {

  sealed trait Permission {
    def id: String
  }

  case object DashboardPermission extends Permission {
    override val id: String = "Dashboard Permission"
  }

}

