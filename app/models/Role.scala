package models

import java.util.UUID

import play.api.libs.json.{Json, Reads, Writes}

sealed trait LWMRole {
  def label: String
}

case class Role(label: String, id: UUID = UUID.randomUUID) extends LWMRole with UniqueEntity

object Role {

  implicit val writes: Writes[Role] = Json.writes[Role]
  implicit val reads: Reads[Role] = Json.reads[Role]

  case object God extends LWMRole {
    override val label = "God"
  }

  case object Admin extends LWMRole {
    override val label = AdminLabel
  }

  case object EmployeeRole extends LWMRole {
    override val label = EmployeeLabel
  }

  case object StudentRole extends LWMRole {
    override val label = StudentLabel
  }

  case object CourseEmployee extends LWMRole {
    override val label = CourseEmployeeLabel
  }

  case object CourseAssistant extends LWMRole {
    override val label = CourseAssistantLabel
  }

  case object CourseManager extends LWMRole {
    override val label = CourseManagerLabel
  }

  case object RightsManager extends LWMRole {
    override val label = RightsManagerLabel
  }

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