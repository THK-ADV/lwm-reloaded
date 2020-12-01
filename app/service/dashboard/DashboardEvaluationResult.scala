package service.dashboard

import play.api.libs.json.{Writes, Json}

case class DashboardEvaluationResult(
  course: String,
  semester: String,
  passed: Boolean,
  bonus: Int
)

object DashboardEvaluationResult {
  implicit def writes: Writes[DashboardEvaluationResult] = Json.writes[DashboardEvaluationResult]
}
