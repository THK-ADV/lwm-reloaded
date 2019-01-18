package models

import java.sql.Timestamp
import java.util.UUID

import org.joda.time.DateTime
import play.api.libs.json.{Json, Reads, Writes}
import utils.LwmDateTime.DateTimeConverter

case class PostgresRole(label: String, id: UUID = UUID.randomUUID) extends UniqueEntity

case class RoleDb(label: String, lastModified: Timestamp = DateTime.now.timestamp, invalidated: Option[Timestamp] = None, id: UUID = UUID.randomUUID) extends UniqueDbEntity {
  def toLwmModel = PostgresRole(label, id)
}

object PostgresRole {
  implicit val writes: Writes[PostgresRole] = Json.writes[PostgresRole]
  implicit val reads: Reads[PostgresRole] = Json.reads[PostgresRole]
}

sealed trait Role {
  def label: String
}

object Role {

  case object God extends Role {
    override val label = "God"
  }

  case object Admin extends Role {
    override val label = Roles.AdminLabel
  }

  case object Employee extends Role {
    override val label = Roles.EmployeeLabel
  }

  case object Student extends Role {
    override val label = Roles.StudentLabel
  }

  case object CourseEmployee extends Role {
    override val label = Roles.CourseEmployeeLabel
  }

  case object CourseAssistant extends Role {
    override val label = Roles.CourseAssistantLabel
  }

  case object CourseManager extends Role {
    override val label = Roles.CourseManagerLabel
  }

  case object RightsManager extends Role {
    override val label = Roles.RightsManagerLabel
  }

}

object Roles {
  lazy val AdminLabel = "Administrator"
  lazy val EmployeeLabel = "Mitarbeiter"
  lazy val StudentLabel = "Student"
  lazy val CourseEmployeeLabel = "Modulmitarbeiter"
  lazy val CourseAssistantLabel = "Hilfskraft"
  lazy val CourseManagerLabel = "Modulverantwortlicher"
  lazy val RightsManagerLabel = "Rechteverantwortlicher"

  lazy val all = List(
    AdminLabel,
    EmployeeLabel,
    StudentLabel,
    CourseEmployeeLabel,
    CourseAssistantLabel,
    CourseManagerLabel,
    RightsManagerLabel
  )

  def fromUserStatus(status: String): String = status match {
    case User.EmployeeType => EmployeeLabel
    case User.LecturerType => EmployeeLabel
    case User.StudentType => StudentLabel
  }
}