package models

import java.sql.Timestamp
import java.util.UUID

import controllers.JsonSerialisation
import org.joda.time.DateTime
import play.api.libs.json.{JsPath, Json, Reads, Writes}
import play.api.libs.functional.syntax._
import utils.Ops.JsPathX
import models.LwmDateTime.DateTimeConverter

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

sealed trait Role extends UniqueEntity

case class PostgresRole(label: String, permissions: Set[UUID], id: UUID = UUID.randomUUID) extends Role

case class RoleDb(label: String, permissions: Set[UUID], lastModified: Timestamp = DateTime.now.timestamp, invalidated: Option[Timestamp] = None, id: UUID = UUID.randomUUID) extends UniqueDbEntity {
  def toLwmModel = PostgresRole(label, permissions, id)
}

case class PostgresRoleProtocol(label: String, permissions: Set[UUID])

case class PostgresRoleAtom(label: String, permissions: Set[PostgresPermission], id: UUID) extends Role

case class RolePermission(role: UUID, permission: UUID, id: UUID = UUID.randomUUID) extends UniqueEntity

object Role {

  implicit def writes: Writes[Role] = new Writes[Role] {
    override def writes(o: Role) = o match {
      case postgresRole: PostgresRole => Json.toJson(postgresRole)(PostgresRole.writes)
      case postgresRoleAtom: PostgresRoleAtom => Json.toJson(postgresRoleAtom)(PostgresRoleAtom.writesAtom)
    }
  }
}

object SesameRole extends UriGenerator[SesameRole] with JsonSerialisation[SesameRoleProtocol, SesameRole, SesameRole] {

  override implicit def reads: Reads[SesameRoleProtocol] = Json.reads[SesameRoleProtocol]

  override def writesAtom: Writes[SesameRole] = writes

  override implicit def writes: Writes[SesameRole] = Json.writes[SesameRole]

  override def base: String = "roles"
}

object PostgresRole extends JsonSerialisation[PostgresRoleProtocol, PostgresRole, PostgresRoleAtom] {

  override implicit def reads: Reads[PostgresRoleProtocol] = Json.reads[PostgresRoleProtocol]

  override implicit def writes: Writes[PostgresRole] = Json.writes[PostgresRole]

  override implicit def writesAtom: Writes[PostgresRoleAtom] = PostgresRoleAtom.writesAtom
}

object PostgresRoleAtom {

  implicit def writesAtom: Writes[PostgresRoleAtom] = (
    (JsPath \ "label").write[String] and
      (JsPath \ "permissions").writeSet[PostgresPermission](PostgresPermission.writes) and
      (JsPath \ "id").write[UUID]
    ) (unlift(PostgresRoleAtom.unapply))
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

object RoleDb{
  def from(protocol: PostgresRoleProtocol, existingId: Option[UUID]) = {
    RoleDb(protocol.label, protocol.permissions, DateTime.now.timestamp, None, existingId.getOrElse(UUID.randomUUID))
  }
}