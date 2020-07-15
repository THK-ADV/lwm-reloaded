package utils.student_query_engine

sealed trait Operator

object Operator {

  object And extends Operator

  object Or extends Operator

  def apply(string: String): Option[Operator] = string match {
    case "and" => Some(And)
    case "or" => Some(Or)
    case _ => None
  }

  def values(): List[String] = List("and", "or")
}