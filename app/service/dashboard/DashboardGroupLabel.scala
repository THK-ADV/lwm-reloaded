package service.dashboard

import play.api.libs.json.{Json, Writes}

case class DashboardGroupLabel(
  groupLabel: String,
  labworkLabel: String
)

object DashboardGroupLabel {
  implicit def writes: Writes[DashboardGroupLabel] = Json.writes[DashboardGroupLabel]
}
