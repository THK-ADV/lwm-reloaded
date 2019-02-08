package database.helper

import scala.util.{Failure, Success, Try}

sealed trait LdapUserStatus {
  def label: String
}

object LdapUserStatus {

  def apply(label: String): Try[LdapUserStatus] = label match {
    case EmployeeStatus.label => Success(EmployeeStatus)
    case LecturerStatus.label => Success(LecturerStatus)
    case StudentStatus.label => Success(StudentStatus)
    case _ => Failure(new Throwable(s"status must be either ${EmployeeStatus.label}, ${LecturerStatus.label} or ${StudentStatus.label}, but was $label"))
  }
}

case object EmployeeStatus extends LdapUserStatus {
  override val label = "employee"
}

case object LecturerStatus extends LdapUserStatus {
  override val label = "lecturer"
}

case object StudentStatus extends LdapUserStatus {
  override val label = "student"
}
