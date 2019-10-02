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
    override val label = "Administrator"
  }

  case object EmployeeRole extends LWMRole {
    override val label = "Mitarbeiter"
  }

  case object StudentRole extends LWMRole {
    override val label = "Student"
  }

  case object CourseEmployee extends LWMRole {
    override val label = "Modulmitarbeiter"
  }

  case object CourseAssistant extends LWMRole {
    override val label = "Hilfskraft"
  }

  case object CourseManager extends LWMRole {
    override val label = "Modulverantwortlicher"
  }

  lazy val all: List[LWMRole] = List(
    Admin,
    EmployeeRole,
    StudentRole,
    CourseEmployee,
    CourseAssistant,
    CourseManager
  )
}