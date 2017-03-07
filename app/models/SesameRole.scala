package models

import java.util.UUID

import controllers.JsonSerialisation
import org.joda.time.DateTime
import play.api.libs.json.{Json, Reads, Writes}

/**
  * Structure abstracting over a set of unary `Permission`s.
  * These sets are aggregated to specific `Role`s such that default, reusable `Role`s are possible.
  * `Role`s are independent. They can only be referenced by other graphs.
  *
  * @param label       Name or label of the `Role`
  * @param permissions The unary permissions of that `Role`
  */

case class SesameRole(label: String, permissions: Set[Permission], invalidated: Option[DateTime] = None, id: UUID = SesameRole.randomUUID) extends UniqueEntity

case class SesameRoleProtocol(label: String, permissions: Set[Permission])

case class PostgresRole(label: String, id: UUID = PostgresRole.randomUUID) extends UniqueEntity

object SesameRole extends UriGenerator[SesameRole] with JsonSerialisation[SesameRoleProtocol, SesameRole, SesameRole] {

  override implicit def reads: Reads[SesameRoleProtocol] = Json.reads[SesameRoleProtocol]

  override def writesAtom: Writes[SesameRole] = writes

  override implicit def writes: Writes[SesameRole] = Json.writes[SesameRole]

  override def base: String = "roles"
}

object PostgresRole extends UriGenerator[PostgresRole] with JsonSerialisation[PostgresRole, PostgresRole, PostgresRole] {

  override implicit def reads: Reads[PostgresRole] = Json.reads[PostgresRole]

  override implicit def writes: Writes[PostgresRole] = Json.writes[PostgresRole]

  override implicit def writesAtom: Writes[PostgresRole] = writes

  override def base: String = "roles"
}

object Roles {

  lazy val AdminLabel = "Administrator"
  lazy val EmployeeLabel = "Mitarbeiter"
  lazy val StudentLabel = "Student"
  lazy val CourseEmployeeLabel = "Modulmitarbeiter"
  lazy val CourseAssistantLabel = "Hilfskraft"
  lazy val CourseManagerLabel = "Modulverantwortlicher"
  lazy val RightsManagerLabel = "Rechteverantwortlicher"
}