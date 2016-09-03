package store.sparql

import org.openrdf.model.Value
import org.openrdf.query.parser.ParsedTupleQuery
import org.openrdf.repository.RepositoryConnection
import org.w3.banana.sesame.{Sesame, SesameModule}
import scala.language.higherKinds
import scala.util.{Failure, Success, Try}
import scalaz.{Functor, Monad, Traverse, ~>}

object QueryEngine {
  def empty(implicit qe: QueryExecutor[SelectClause]) = Initial[Nothing, Nothing](select(""))(qe)
}

sealed trait QueryExecutor[A] {
  def parse(s: A): Try[Sesame#SelectQuery]
  def execute(query: Sesame#SelectQuery): Try[Map[String, List[Value]]]
}

sealed trait QueryEngine[F[_], A] {
  def run: Try[F[A]] = this match {
    case Transitional(t) => t
    case a@Initial(_) => Failure(new Throwable("Query cannot be run at this stage. Try selecting specific elements from the Map and then running it."))
  }
}

case class Transitional[F[_], A](T: Try[F[A]]) extends QueryEngine[F, A] {
  import utils.Ops._
  import utils.Ops.MonadInstances.tryM
  import utils.Ops.TraverseInstances.travT

  def map[B](f: A => B)(implicit F: Functor[F]): Transitional[F, B] = Transitional(T map (FF => F.map(FF)(f)))
  def flatMap[G[_], B](f: A => F[B])(implicit M: Monad[F]): Transitional[F, B] = Transitional(T map (FF => M.bind(FF)(f)))

  def changeTo[G[_], B](f: A => G[B])(implicit M: Monad[G], NT: F ~> G): Transitional[G, B] = transform(NT andThen (M.bind(_)(f)))
  def transform[G[_], B](f: F[A] => G[B]): Transitional[G, B] = Transitional(T map f)

  def request[G[_], B](f: A => Try[G[B]])(implicit M: Monad[G], T: Traverse[G], NT: F ~> G): Transitional[G, B] = {
    requestAll(F => M.bind(NT(F))(a => f(a).sequenceM).sequenceM)
  }
  def requestAll[G[_], B](f: F[A] => Try[G[B]]): Transitional[G, B] = Transitional(T flatMap f)
}

case class Initial[F[_], A](z: SelectClause)(implicit qe: QueryExecutor[SelectClause]) extends QueryEngine[F, A] {
  def select[G[_], B](f: Map[String, List[Value]] => G[B]): Transitional[G, B] = Transitional(qe.parse(z) flatMap qe.execute flatMap (f andThen Success.apply))
}

trait SPARQLQueryEngine { self: SesameModule =>
  def connect[A](f: RepositoryConnection => Try[A]): Try[A]

  private implicit val qe: QueryExecutor[SelectClause] = new QueryExecutor[SelectClause] {
    import self.rdfStore.sparqlEngineSyntax._

    override def execute(query: ParsedTupleQuery): Try[Map[String, List[Value]]] = connect { conn =>
      conn.executeSelect(query) map { solutions =>
        import scala.collection.JavaConversions._
        solutions.foldRight(Map[String, List[Value]]()){ (l1, r1) =>
          l1.getBindingNames.toVector.foldRight(r1) { (l2, r2) =>
            r2.get(l2) match {
              case Some(vl) => r2.updated(l2, vl :+ l1.getValue(l2))
              case None => r2 + (l2 -> List(l1.getValue(l2)))
            }
          }
        }
      }
    }

    override def parse(s: SelectClause): Try[ParsedTupleQuery] = self.sparqlOps.parseSelect(s.run)
  }

  def prepareQuery[F[_], A](s: SelectClause): Initial[F, A] = Initial[F, A](s)
}
