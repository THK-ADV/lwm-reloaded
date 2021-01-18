package utils.student_query_engine

sealed trait Key {
  def value: String
}

object Key {

  def apply(value: String): Option[Key] = value match {
    case "course" => Some(Course)
    case "labwork" => Some(Labwork)
    case "semester" => Some(Semester)
    case "lastname" => Some(Lastname)
    case "degree" => Some(Degee)
    case "passedAssignment" => Some(PassedAssignment)
    case "failedAssignment" => Some(FailedAssignment)
    case _ => None
  }

  def values(): List[String] = List(
    "semester",
    "degree",
    "course",
    "labwork",
    "lastname",
    "passedAssignment",
    "failedAssignment",
  )

  object Lastname extends Key {
    override val value = "lastname"
  }

  object Labwork extends Key {
    override val value = "labwork"
  }

  object Degee extends Key {
    override val value = "degree"
  }

  object PassedAssignment extends Key {
    override val value = "passedAssignment"
  }

  object FailedAssignment extends Key {
    override val value = "failedAssignment"
  }

  object Course extends Key {
    override val value = "course"
  }

  object Semester extends Key {
    override val value = "semester"
  }

}