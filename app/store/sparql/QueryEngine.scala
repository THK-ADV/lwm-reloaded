package store.sparql

import org.openrdf.model.Value
import org.openrdf.query.BindingSet
import org.openrdf.repository.RepositoryConnection
import org.w3.banana.sesame.SesameModule

trait Query[A] {
  self: SesameModule =>

  def selectOperation: QueryOperation[A]

  def askOperation: QueryOperation[Boolean]

  def withConnection[B](f: RepositoryConnection => B): B
}

case class QueryOperation[A](action: String => Option[A]) {

  def <>(q: String): Option[A] = run(q)

  def run: String => Option[A] = q => action(q)

  def map[B](f: A => B): QueryOperation[B] = flatMap(a => new QueryOperation[B](q => run(q) map f))

  def flatMap[B](f: A => QueryOperation[B]) = new QueryOperation[B](q => run(q).flatMap(a => f(a) run q)) //not so stack-safe

  def asOpt: QueryOperation[Option[A]] = new QueryOperation[Option[A]](s => run(s) map Option.apply)

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

  //memoize query
  def extract(s: String) = {
    selectOperation map { v =>
      v.map(_.getValue(s))
    }
  }

  def query(clause: SelectClause) =
    selectOperation.map { v =>
      clause.v.flatMap(varr => v.map(_.getValue(varr.v)))
    } <> clause.run

}
