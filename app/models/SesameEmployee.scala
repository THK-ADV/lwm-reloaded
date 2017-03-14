package models

import java.util.UUID

import controllers.JsonSerialisation
import org.joda.time.DateTime
import play.api.libs.json.{Json, Reads, Writes}

case class SesameEmployee(systemId: String, lastname: String, firstname: String, email: String, status: String, invalidated: Option[DateTime] = None, id: UUID = User.randomUUID) extends User

object SesameEmployee extends JsonSerialisation[SesameEmployee, SesameEmployee, SesameEmployee] {

  lazy val default = SesameEmployee("n.a.", "n.a.", "n.a", "n.a.", User.EmployeeType)

  override implicit def reads: Reads[SesameEmployee] = Json.reads[SesameEmployee]

  override implicit def writes: Writes[SesameEmployee] = Json.writes[SesameEmployee]

  override implicit def writesAtom: Writes[SesameEmployee] = writes
}

case class PostgresEmployee(systemId: String, lastname: String, firstname: String, email: String, id: UUID = User.randomUUID) extends User

object PostgresEmployee extends JsonSerialisation[PostgresEmployee, PostgresEmployee, PostgresEmployee] {

  override implicit def reads: Reads[PostgresEmployee] = Json.reads[PostgresEmployee]

  override implicit def writes: Writes[PostgresEmployee] = Json.writes[PostgresEmployee]

  override implicit def writesAtom: Writes[PostgresEmployee] = ???
}