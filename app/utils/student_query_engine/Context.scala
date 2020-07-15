package utils.student_query_engine

import java.util.UUID

sealed trait Context {
  def id: UUID
}

object Context {

  case class Course(id: UUID) extends Context

  case class Semester(id: UUID) extends Context

  case class Labwork(id: UUID) extends Context

}