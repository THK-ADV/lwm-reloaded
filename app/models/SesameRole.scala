package models

import java.sql.Timestamp
import java.util.UUID

import controllers.JsonSerialisation
import org.joda.time.DateTime
import play.api.libs.json.{JsPath, Json, Reads, Writes}
import play.api.libs.functional.syntax._
import utils.Ops.JsPathX
import utils.LwmDateTime.DateTimeConverter

/**
  * Structure abstracting over a set of unary `Permission`s.
  * These sets are aggregated to specific `Role`s such that default, reusable `Role`s are possible.
  * `Role`s are independent. They can only be referenced by other graphs.
  *
  * @param label       Name or label of the `Role`
  * @param permissions The unary permissions of that `Role`
  */

case class SesameRole(label: String, permissions: Set[SesamePermission], invalidated: Option[DateTime] = None, id: UUID = SesameRole.randomUUID) extends UniqueEntity

case class SesameRoleProtocol(label: String, permissions: Set[SesamePermission])

// Postgres

case class PostgresRole(label: String, id: UUID = UUID.randomUUID) extends UniqueEntity

case class RoleDb(label: String, lastModified: Timestamp = DateTime.now.timestamp, invalidated: Option[Timestamp] = None, id: UUID = UUID.randomUUID) extends UniqueDbEntity {
  def toLwmModel = PostgresRole(label, id)
}

case class PostgresRoleProtocol(label: String)

object SesameRole extends UriGenerator[SesameRole] with JsonSerialisation[SesameRoleProtocol, SesameRole, SesameRole] {

  override implicit def reads: Reads[SesameRoleProtocol] = Json.reads[SesameRoleProtocol]

  override def writesAtom: Writes[SesameRole] = writes

  override implicit def writes: Writes[SesameRole] = Json.writes[SesameRole]

  override def base: String = "roles"
}

object PostgresRole extends JsonSerialisation[PostgresRoleProtocol, PostgresRole, PostgresRole] {

  override implicit def reads: Reads[PostgresRoleProtocol] = Json.reads[PostgresRoleProtocol]

  override implicit def writes: Writes[PostgresRole] = Json.writes[PostgresRole]

  override implicit def writesAtom: Writes[PostgresRole] = writes
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

  def lift(label: String): Option[Role] = Option(label match {
    case God.label => God
    case Admin.label => Admin
    case Employee.label => Employee
    case Student.label => Student
    case CourseEmployee.label => CourseEmployee
    case CourseAssistant.label => CourseAssistant
    case CourseManager.label => CourseManager
    case RightsManager.label => RightsManager
    case _ => null
  })
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

object RoleDb {
  def from(protocol: PostgresRoleProtocol, existingId: Option[UUID]) = {
    RoleDb(protocol.label, DateTime.now.timestamp, None, existingId.getOrElse(UUID.randomUUID))
  }
}