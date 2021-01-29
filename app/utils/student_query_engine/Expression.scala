package utils.student_query_engine

sealed trait Expression

object Expression {

  case class Single(key: Key, value: String) extends Expression

  case class Combined(lhs: Expression, rhs: Expression, operator: Operator) extends Expression

}