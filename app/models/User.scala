package models

import java.util.UUID

import play.api.libs.json.{JsValue, Json, Writes}

trait User extends UniqueEntity {
  def systemId: String
  def lastname: String
  def firstname: String
  def email: String

  def dbUser = this match {
    case s: PostgresStudent =>
      DbUser(s.systemId, s.lastname, s.firstname, s.email, User.studentType, Some(s.registrationId), Some(s.enrollment), s.id)
    case e: PostgresEmployee =>
      DbUser(e.systemId, e.lastname, e.firstname, e.email, User.employeeType, None, None, e.id)
    case l: PostgresLecturer =>
      DbUser(l.systemId, l.lastname, l.firstname, l.email, User.lecturerType, None, None, l.id)
  }
}

object User extends UriGenerator[User] {
  override def base: String = "users"

  lazy val employeeType = "employee"

  lazy val lecturerType = "lecturer"

  lazy val studentType = "student"

  implicit def writes: Writes[User] = new Writes[User] {
    override def writes(user: User): JsValue = user match {
      case sesameStudent: SesameStudent => Json.toJson(sesameStudent)(SesameStudent.writes)
      case sesameEmployee: SesameEmployee => Json.toJson(sesameEmployee)(SesameEmployee.writes)
      case postgresStudent: PostgresStudent => Json.toJson(postgresStudent)(PostgresStudent.writes)
      case postgresEmployee: PostgresEmployee => Json.toJson(postgresEmployee)(PostgresEmployee.writes)
      case postgresLecturer: PostgresLecturer => Json.toJson(postgresLecturer)(PostgresLecturer.writes)
    }
  }
}

final case class DbUser(systemId: String, lastname: String, firstname: String, email: String, status: String, registrationId: Option[String], enrollment: Option[UUID], id: UUID) extends User {

  def user: User = this match {
    case DbUser(sId, last, first, mail, stat, Some(regId), Some(enroll), studentId) if stat == User.studentType =>
      PostgresStudent(sId, last, first, mail, regId, enroll, studentId)
    case DbUser(sId, last, first, mail, stat, None, None, employeeId) if stat == User.employeeType =>
      PostgresEmployee(sId, last, first, mail, employeeId)
    case DbUser(sId, last, first, mail, stat, None, None, lecturerId) if stat == User.lecturerType =>
      PostgresLecturer(sId, last, first, mail, lecturerId)
  }
}