package security

sealed trait LWMRole {
  def label: String
}

object LWMRole {

  sealed trait CourseRelatedRole extends LWMRole

  sealed trait BasicRole extends LWMRole

  case object God extends BasicRole {
    override val label = "God"
  }

  case object Admin extends BasicRole {
    override val label = "Administrator"
  }

  case object EmployeeRole extends BasicRole {
    override val label = "Mitarbeiter"
  }

  case object StudentRole extends BasicRole {
    override val label = "Student"
  }

  case object CourseEmployee extends CourseRelatedRole {
    override val label = "Modulmitarbeiter"
  }

  case object CourseAssistant extends CourseRelatedRole {
    override val label = "Hilfskraft"
  }

  case object CourseManager extends CourseRelatedRole {
    override val label = "Modulverantwortlicher"
  }

  def courseRelated: List[CourseRelatedRole] = List(
    CourseEmployee,
    CourseAssistant,
    CourseManager
  )

  lazy val all: List[LWMRole] = List(
    Admin,
    EmployeeRole,
    StudentRole,
    CourseEmployee,
    CourseAssistant,
    CourseManager
  )
}
