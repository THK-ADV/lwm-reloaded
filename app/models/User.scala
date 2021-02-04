package models

import database.helper.LdapUserStatus
import play.api.libs.json.{JsValue, Json, Reads, Writes}

import java.util.UUID

trait User extends UniqueEntity {
  def systemId: String

  def lastname: String

  def firstname: String

  def email: String

  def status: LdapUserStatus
}

sealed trait UserProtocol

object UserProtocol {

  case class StudentProtocol(systemId: String, lastname: String, firstname: String, email: String, registrationId: String, enrollment: UUID) extends UserProtocol

  case class EmployeeProtocol(systemId: String, lastname: String, firstname: String, email: String) extends UserProtocol

  implicit val readsStudent: Reads[StudentProtocol] = Json.reads[StudentProtocol]

  implicit val readsEmployee: Reads[EmployeeProtocol] = Json.reads[EmployeeProtocol]

  implicit val reads: Reads[UserProtocol] = (json: JsValue) => {
    val enrollment = json.\("enrollment")
    val registrationId = json.\("registrationId")

    if (enrollment.isDefined && registrationId.isDefined) Json.fromJson(json)(readsStudent)
    else Json.fromJson(json)(readsEmployee)
  }
}

object User {

  implicit val writes: Writes[User] = {
    case postgresStudent: Student => Json.toJson(postgresStudent)(Student.writes)
    case postgresStudentAtom: StudentAtom => Json.toJson(postgresStudentAtom)(StudentAtom.writes)
    case postgresEmployee: Employee => Json.toJson(postgresEmployee)(Employee.writes)
    case postgresLecturer: Lecturer => Json.toJson(postgresLecturer)(Lecturer.writes)
  }
}

