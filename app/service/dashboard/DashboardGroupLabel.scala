package service.dashboard

import java.util.UUID

import play.api.libs.json.{Json, Writes}

case class DashboardGroupLabel(
  groupLabel: String,
  labworkLabel: String,
  labworkId: UUID
)

object DashboardGroupLabel {
  implicit def writes: Writes[DashboardGroupLabel] = Json.writes[DashboardGroupLabel]
}
