package models

import java.util.UUID

import org.joda.time.DateTime


case class Session(user: String, id: UUID = UUID.randomUUID(), expirationDate: DateTime = DateTime.now().plusDays(1))

object Permissions {

  sealed trait Permission {
    def id: String
  }

  case object DashboardPermission extends Permission {
    override val id: String = "Dashboard Permission"
  }

}

