package service.dashboard

case class DashboardConfig(
  atomic: Boolean,
  numberOfUpcomingElements: Option[Int],
  entriesSinceNow: Option[Boolean],
  sortedByDate: Option[Boolean],
  ownEntriesOnly: Option[Boolean],
)
