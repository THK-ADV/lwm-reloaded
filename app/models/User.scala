package models

import java.util.UUID

import org.joda.time.DateTime
import play.api.libs.json.{JsValue, Json, Reads, Writes}

trait User extends UniqueEntity {
  def systemId: String
  def lastname: String
  def firstname: String
  def email: String

  def toDbUser = this match {
    case s: PostgresStudent =>
      DbUser(s.systemId, s.lastname, s.firstname, s.email, User.StudentType, Some(s.registrationId), Some(s.enrollment), None, s.id)
    case e: PostgresEmployee =>
      DbUser(e.systemId, e.lastname, e.firstname, e.email, User.EmployeeType, None, None, None, e.id)
    case l: PostgresLecturer =>
      DbUser(l.systemId, l.lastname, l.firstname, l.email, User.LecturerType, None, None, None, l.id)
  }
}

object User extends UriGenerator[User] {
  override def base: String = "users"

  lazy val EmployeeType = "employee"
  lazy val LecturerType = "lecturer"
  lazy val StudentType = "student"

  lazy val types = List(EmployeeType, LecturerType, StudentType)

  implicit def writes: Writes[User] = new Writes[User] {
    override def writes(user: User): JsValue = user match {
      case sesameStudent: SesameStudent => Json.toJson(sesameStudent)(SesameStudent.writes)
      case sesameEmployee: SesameEmployee => Json.toJson(sesameEmployee)(SesameEmployee.writes)
      case postgresStudent: PostgresStudent => Json.toJson(postgresStudent)(PostgresStudent.writes)
      case postgresStudentAtom: PostgresStudentAtom => Json.toJson(postgresStudentAtom)(PostgresStudentAtom.writesAtom)
      case postgresEmployee: PostgresEmployee => Json.toJson(postgresEmployee)(PostgresEmployee.writes)
      case postgresLecturer: PostgresLecturer => Json.toJson(postgresLecturer)(PostgresLecturer.writes)
    }
  }

  case class UserProtocol(systemId: String)

  implicit def reads: Reads[UserProtocol] = Json.reads[UserProtocol]
}

case class DbUser(systemId: String, lastname: String, firstname: String, email: String, status: String, registrationId: Option[String], enrollment: Option[UUID], invalidated: Option[DateTime], id: UUID) extends UniqueEntity {

  def toUser: User = this match {
    case DbUser(sId, last, first, mail, stat, Some(regId), Some(enroll), _, studentId) if stat == User.StudentType =>
      PostgresStudent(sId, last, first, mail, regId, enroll, studentId)
    case DbUser(sId, last, first, mail, stat, None, None, _, employeeId) if stat == User.EmployeeType =>
      PostgresEmployee(sId, last, first, mail, employeeId)
    case DbUser(sId, last, first, mail, stat, None, None, _ ,lecturerId) if stat == User.LecturerType =>
      PostgresLecturer(sId, last, first, mail, lecturerId)
  }
}