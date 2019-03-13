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

    def toUniqueEntity: UniqueEntity
  }

  implicit val uniqueEntityWrites: Writes[UniqueEntity] = {
    case u: User => User.writes.writes(u)
    case a: AssignmentPlanLike => AssignmentPlanLike.writes.writes(a)
    case c: CourseLike => CourseLike.writes.writes(c)
    case d: Degree => Degree.writes.writes(d)
    case l: LabworkApplicationLike => LabworkApplicationLike.writes.writes(l)
    case l: LabworkLike => LabworkLike.writes.writes(l)
    case r: Role => Role.writes.writes(r)
    case r: Room => Room.writes.writes(r)
    case s: Semester => Semester.writes.writes(s)
    case t: TimetableLike => TimetableLike.writes.writes(t)
    case b: Blacklist => Blacklist.writes.writes(b)
    case r: ReportCardEntry => ReportCardEntry.writes.writes(r)
    case a: AuthorityLike => AuthorityLike.writes.writes(a)
    case s: ScheduleEntryLike => ScheduleEntryLike.writes.writes(s)
    case g: GroupLike => GroupLike.writes.writes(g)
    case r: ReportCardEvaluationLike => ReportCardEvaluationLike.writes.writes(r)
    case p: ReportCardEvaluationPattern => ReportCardEvaluationPattern.writes.writes(p)
  }

  object UniqueEntity {
    def toJson[A <: UniqueEntity](seq: List[A]*)(implicit writes: Writes[A]): Seq[JsValue] = seq.map(a => Json.toJson(a))
  }

}
