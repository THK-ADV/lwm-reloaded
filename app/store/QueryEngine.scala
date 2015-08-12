package store

import org.openrdf.query.BindingSet
import org.openrdf.repository.RepositoryConnection

trait Query[A] {
  self: APIModule =>
  type QURI = String
  type QNode = String

  def resource(s: String): QURI = resource(ops.makeUri(s))

  def resource(uri: Rdf#URI): QURI = s"<$uri>"

  def literal(value: String): QNode = s""""$value""""

  def select: QueryOperation[A]

  def ask: QueryOperation[Boolean]

  def withConnection[A](f: RepositoryConnection => A): A
}

case class QueryOperation[A](action: String => Option[A]) {

  def >>(q: String): Option[A] = run(q)

  def run: String => Option[A] = q => action(q)

  def map[B](f: A => B): QueryOperation[B] = flatMap(a => new QueryOperation[B](q => run(q) map f))

  def flatMap[B](f: A => QueryOperation[B]) = new QueryOperation[B](q => run(q).flatMap(a => f(a) run q))

  def asOpt: QueryOperation[Option[A]] = new QueryOperation[Option[A]](s => run(s) map Option.apply)

}

trait QueryEngine[A] extends Query[A] {
  self: APIModule =>
}

trait SPARQLQueryEngine extends QueryEngine[Vector[BindingSet]] {
  self: APIModule =>

  override def select: QueryOperation[Vector[BindingSet]] = {
    import rdfStore._
    import sparqlOps._
    QueryOperation(s =>
      parseSelect(s).flatMap { q =>
        withConnection { connection =>
          executeSelect(connection, q, Map()).map(_.asInstanceOf[Vector[BindingSet]])
        }
      }.toOption)
  }

  override def ask: QueryOperation[Boolean] = {
    import rdfStore._
    import sparqlOps._
    QueryOperation(s => parseAsk(s).flatMap { q =>
      withConnection { connection =>
        executeAsk(connection, q, Map())
      }
    }.toOption)
  }
}
