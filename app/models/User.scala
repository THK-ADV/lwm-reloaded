package models

import play.api.libs.json.{Json, Reads, Writes}

trait User extends UniqueEntity {
  def systemId: String

  def lastname: String

  def firstname: String

  def email: String
}

case class UserProtocol(systemId: String)

object User {

  implicit val writes: Writes[User] = {
    case postgresStudent: Student => Json.toJson(postgresStudent)(Student.writes)
    case postgresStudentAtom: StudentAtom => Json.toJson(postgresStudentAtom)(StudentAtom.writes)
    case postgresEmployee: Employee => Json.toJson(postgresEmployee)(Employee.writes)
    case postgresLecturer: Lecturer => Json.toJson(postgresLecturer)(Lecturer.writes)
  }
}

object UserProtocol {
  implicit val reads: Reads[UserProtocol] = Json.reads[UserProtocol]
}