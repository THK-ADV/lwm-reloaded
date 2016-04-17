package store.sparql

object SDSL {

}

//TODO: EXPAND FURTHER
/**
 * Abstract type of a query clause and its underlying interpretation.
 *
 * The Clauses are manipulated as a head :: tail type structure.
 * It is built analogous to typical recursive Cons- data structures, where
 * a Cons-type and a None- or Nil-type describe the "full" and "empty"
 * variations of the structure.
 **/
sealed trait Clause {
  def run: String = foldRight("") { (clause, acc) =>
    clause match {
      case AskClause(body, t) => acc + "ASK { " + body.run + "}"
      case SelectClause(v, t) => acc + s"SELECT ${v.mkString(" ")} "
      case DistinctClause(v, t) => acc + s"DISTINCT ${v.mkString(" ")} "
      case EverythingClause(t) => acc + "* "
      case WhereClause(body, t) => acc + s"WHERE { " + body.run + " } "
      case StatementClause(ss, t) => acc + s"${ss._1} ${ss._2} ${ss._3} . "
      case FilterClause(p, t) => acc + s"FILTER ($p) . "
      case FilterNotExistsClause(body, t) => acc + s"FILTER NOT EXISTS { " + body.run + "} ."
      case OptionalClause(body, t) => acc + "OPTIONAL { " + body.run + "} ."
      case OrderByClause(o, t) => acc + s"ORDER BY ${o getOrElse ""} "
      case AscendingClause(v, t) => acc + s"ORDER BY ASC($v) "
      case DescendingClause(v, t) => acc + s"ORDER BY DESC($v)"
      case GroupByClause(p, t) => acc + s"GROUP BY $p "
      case FilterStartsWith(w, s, t) => acc + s"FILTER STRSTARTS(str($w), $s)"
      case _ => acc
    }
  }

  @annotation.tailrec
  final def foldLeft[A](z: A)(f: (A, Clause) => A): A = this match {
    case c: ConsClause => c.tail.foldLeft(f(z, c))(f)
    case _ => z
  }

  // Not really `foldRight`, but a reversed `foldLeft`. It simulates the signature of `foldRight`.
  // Because the algebraic structure is not directly reversible, foldRight would need explicit recursion.
  // This avoids using it, keeping the implementation stack-safe.
  final def foldRight[A](z: A)(f: (Clause, A) => A): A = foldLeft(z)((a, c) => f(c, a))

  def append(c: Clause): Clause
}

case object NoneClause extends Clause {
  override def append(c: Clause): Clause = c
}

trait ConsClause extends Clause {
  def tail: Clause
}

//--- The current algebra
case class SelectClause(v: Vector[Properties#Var] = Vector.empty[Properties#Var], tail: Clause = NoneClause) extends ConsClause {
  override def append(c: Clause): SelectClause = SelectClause(v, tail append c)
}

case class DistinctClause(v: Vector[Properties#Var], tail: Clause = NoneClause) extends ConsClause {
  override def append(c: Clause): Clause = DistinctClause(v, tail append c)
}

case class WhereClause(body: Clause, tail: Clause = NoneClause) extends ConsClause {
  override def append(c: Clause): Clause = WhereClause(body, tail append c)
}

case class StatementClause[A <: Properties#Property](statement: (A, A, A), tail: Clause = NoneClause) extends ConsClause {
  override def append(c: Clause) = StatementClause(statement, tail append c)
}

case class FilterClause(p: String, tail: Clause = NoneClause) extends ConsClause {
  override def append(c: Clause): Clause = FilterClause(p, tail append c)
}

case class FilterNotExistsClause(body: Clause, tail: Clause = NoneClause) extends ConsClause {
  override def append(c: Clause): Clause = FilterNotExistsClause(body, tail append c)
}

case class OptionalClause(body: Clause, tail: Clause = NoneClause) extends ConsClause {
  override def append(c: Clause): Clause = OptionalClause(body, tail append c)
}

case class OrderByClause(v: Option[Properties#Var] = None, tail: Clause = NoneClause) extends ConsClause {
  override def append(c: Clause): Clause = OrderByClause(v, tail append c)
}

case class AscendingClause(v: Properties#Var, tail: Clause = NoneClause) extends ConsClause {
  override def append(c: Clause): Clause = AscendingClause(v, tail append c)
}

case class DescendingClause(v: Properties#Var, tail: Clause = NoneClause) extends ConsClause {
  override def append(c: Clause): Clause = DescendingClause(v, tail append c)
}

case class GroupByClause(v: Properties#Var, tail: Clause = NoneClause) extends ConsClause {
  override def append(c: Clause): Clause = GroupByClause(v, tail append c)
}

case class EverythingClause(tail: Clause = NoneClause) extends ConsClause {
  override def append(c: Clause): Clause = EverythingClause(tail append c)
}

case class AskClause(body: Clause, tail: Clause = NoneClause) extends ConsClause {
  override def append(c: Clause): Clause = AskClause(body, tail append c)
}

case class FilterStartsWith(what: String, startsWith: String, tail: Clause = NoneClause) extends ConsClause {
  override def append(c: Clause): Clause = FilterStartsWith(what, startsWith, c)
}
//---

/**
 * DSL backbone for a SELECT operation.
 */
trait SelectOperation extends Clauses with Properties {
  def apply(args: String*) = SelectClause((args map v).toVector)

  def distinct(args: String*) = SelectClause() append DistinctClause((args map v).toVector)

  def * = SelectClause() append EverythingClause()

  implicit class SelectOps(select: SelectClause) {
    def where(nc: Clause) = select append WhereClause(nc)

    def orderby(elm: String = "") = select append {
        if (elm == "") OrderByClause()
        else OrderByClause(Some(v(elm)))
      }


    def groupby(elm: String) = select append GroupByClause(v(elm))

    def asc(elm: String) = select append AscendingClause(v(elm))

    def desc(elm: String) = select append DescendingClause(v(elm))
  }

}

/**
 * DSL backbone for an ASK operation.
 */
trait AskOperation extends Clauses with Properties {
  def apply(c: Clause): AskClause = AskClause(c)
}

/**
 * DSL backbone for any Clause.
 *
 * More concrete operations that need to support further clause nesting and operations must
 * mixin this trait.
 */
trait Clauses {
  def ^(s: Properties#Property, p: Properties#Property, o: Properties#Property): StatementClause[Properties#Property] = StatementClause((s, p, o))

  implicit class ClauseOps(c: Clause) {

    def ^(s: Properties#Property, p: Properties#Property, o: Properties#Property) = c append StatementClause((s, p, o))

    def filter(v: String) = c append FilterClause(v)

    def filterStrStarts(what: Properties#Var, starting: String) = c append FilterStartsWith(what.toString, s"'$starting'")

    def filterNotExists(cc: Clause) = c append FilterNotExistsClause(cc)

    def optional(cc: Clause) = c append OptionalClause(cc)
  }

}

/**
 * It defines types and helper functions that amass
 * to type constraints within clauses.
 *
 * These are defined to concretize the DSL usage,
 * and normalise inputs such that they conform to
 * the SPARQL specification.
 */
trait Properties {

  trait Property {
    def >(that: Property) = this + " > " + that

    def <(that: Property) = this + " < " + that

    def >=(that: Property) = this + " >= " + that

    def <=(that: Property) = this + " <= " + that

    def ==(that: Property) = this + " = " + that

    def !=(that: Property) = this + " != " + that

    def ||(that: Property) = this + " || " + that

    def &&(that: Property) = this + " && " + that

    def as(elm: String) = s"($this as ${Var(elm)})"
  }

  case class Res(v: String) extends Property {
    override def toString: String = s"<$v>"
  }

  case class Lit(v: String) extends Property {
    override def toString: String = s""""$v""""
  }

  case class Var(v: String) extends Property {
    override def toString: String = s"?$v"
  }

  /**
   * Normalises input to a `Res` (Resource), viewed semantically
   * as a subject with the following representation: <`subject`>
   *
   * It normalises based on the `String` representation of the input value `A`
    *
    * @tparam A input value
   * @return Resource
   */
  def s[A]: A => Res = s => Res(s.toString)

  /**
   * Normalises input to a `Res` (Resource), viewed semantically
   * as a predicate with the following representation: <`predicate`>
   *
   * It normalises based on the `String` representation of the input value `A`
    *
    * @tparam A input value
   * @return Resource
   */
  def p[A]: A => Res = s => Res(s.toString)

  /**
   * Normalises input to either a `Res` (Resource) or `Lit` (Literal),
   * depending on how the underlying `String` representation of the input value `A`
   * is structured.
   *
   * It is viewed semantically as either a subject or an object (with the following
   * representation: "`object`")
    *
    * @tparam A input value
   * @return Resource or Literal
   */
  def o[A]: A => Property = s => if (s.toString.contains("http")) Res(s.toString) else Lit(s.toString)

  /**
   * Normalises input to a `Var` (Variable), viewed semantically as a
   * variable with the following representation: ?`variable`
   *
   * It normalises based on the `String` representation of the input value `A`
    *
    * @tparam A input value
   * @return Variable
   */
  def v[A]: A => Var = s => Var(s.toString)
}

object select extends SelectOperation
object ask extends AskOperation
