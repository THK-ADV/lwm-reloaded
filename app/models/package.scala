import java.sql.Timestamp
import java.util.UUID

import play.api.libs.json.{JsValue, Json, Writes}

package object models {

  trait UniqueEntity {
    def id: UUID
  }

  trait UniqueDbEntity extends UniqueEntity {
    def lastModified: Timestamp
    def invalidated: Option[Timestamp]

    def toLwmModel: UniqueEntity
  }

  implicit val uniqueEntityWrites: Writes[UniqueEntity] = new Writes[UniqueEntity] {
    override def writes(o: UniqueEntity) = o match {
      case u: User => User.writes.writes(u)
      case a: AssignmentPlan => AssignmentPlan.writes.writes(a)
      case c: Course => Course.writes.writes(c)
      case d: PostgresDegree => PostgresDegree.writes.writes(d)
      case l: LabworkApplication => LabworkApplication.writes.writes(l)
      case l: Labwork => Labwork.writes.writes(l)
      case r: PostgresRole => PostgresRole.writes.writes(r)
      case r: PostgresRoom => PostgresRoom.writes.writes(r)
      case s: PostgresSemester => PostgresSemester.writes.writes(s)
      case t: Timetable => Timetable.writes.writes(t)
      case b: PostgresBlacklist => PostgresBlacklist.writes.writes(b)
      case r: ReportCardEntry => ReportCardEntry.writes.writes(r)
      case a: Authority => Authority.writes.writes(a)
      case s: ScheduleEntry => ScheduleEntry.writes.writes(s)
      case g: Group => Group.writes.writes(g)
      case r: ReportCardEvaluation => ReportCardEvaluation.writes.writes(r)
      case p: ReportCardEvaluationPattern => ReportCardEvaluationPattern.writes.writes(p)
    }
  }

  object UniqueEntity {
    def toJson[A <: UniqueEntity](seq: List[A]*)(implicit writes: Writes[A]): Seq[JsValue] = seq.map(a => Json.toJson(a))
  }
}
