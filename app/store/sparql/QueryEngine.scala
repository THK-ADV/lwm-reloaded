package store.sparql

import org.openrdf.model.Value
import org.openrdf.query.BindingSet
import org.openrdf.repository.RepositoryConnection
import org.w3.banana.sesame.SesameModule

trait Query[A] {
  self: SesameModule =>
  /**
   * Initiates a SELECT `QueryOperation` that automatically checks the validity of the input SELECT query.
   *
   * @return QueryOperation[A] Monad encapsulation of what actions should be done with the query result
   */
  def selectOperation: QueryOperation[A]

  /**
   * Analogous to the `selectOperation` but as an ASK query.
   * @return Boolean result of the ask operation
   */
  def askOperation: QueryOperation[Boolean]

  /**
   * Provider of a `RepositoryConnection`.
   * This is used as a closure in relation with `QueryOperation`s, in order to capture
   * a connection for late evaluation.
   * @param f Consumer function
   * @tparam B Result type of `f`
   * @return Result of `f`
   */
  def withConnection[B](f: RepositoryConnection => B): B
}

case class QueryOperation[A](action: String => Option[A]) {

  def <>(q: String): Option[A] = run(q)

  def run: String => Option[A] = q => action(q)

  def map[B](f: A => B): QueryOperation[B] = flatMap(a => new QueryOperation[B](q => run(q) map f))

  def flatMap[B](f: A => QueryOperation[B]) = new QueryOperation[B](q => run(q).flatMap(a => f(a) run q)) //not so stack-safe

}

trait QueryEngine[A] extends Query[A] {
  self: SesameModule =>
}

trait SPARQLQueryEngine extends QueryEngine[Vector[BindingSet]] {
  self: SesameModule =>

  import rdfStore._
  import sparqlOps._

  override def selectOperation: QueryOperation[Vector[BindingSet]] = {
    QueryOperation(s =>
      parseSelect(s).flatMap { q =>
        withConnection { connection =>
          executeSelect(connection, q, Map())
        }
      }.toOption)
  }

  override def askOperation: QueryOperation[Boolean] = {
    QueryOperation(s => parseAsk(s).flatMap { q =>
      withConnection { connection =>
        executeAsk(connection, q, Map())
      }
    }.toOption)
  }

  /**
   * Inverts the right-associativity of a QueryOperation, to a left-associativity. (lazy -> eager)
   *
   * This means that the following:
   * `QueryOperation.flatMap(f) <> queryString`
   * is inverted to:
   * `query(queryString).flatMap(f)`
   *
   * A helper function directly integrating the SPARQL-DSL.
   * @param clause SelectClause to be run
   * @return QueryOperation Monad encapsulating the result
   */

  def query(clause: SelectClause) =
    selectOperation.map { v =>
      clause.v.flatMap(varr => v.map(_.getValue(varr.v)))
    } <> clause.run

}
