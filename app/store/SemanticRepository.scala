package store

import java.io.File

import info.aduna.iteration.Iterations
import models.{UniqueEntity, UriGenerator}
import org.openrdf.model.{Statement, URI}
import org.openrdf.repository.{RepositoryConnection, RepositoryResult}
import org.openrdf.repository.sail.SailRepository
import org.openrdf.sail.memory.MemoryStore
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame._
import org.w3.banana.{PointedGraph, _}
import store.bind.BindingsS.CompositeClassUris
import store.sparql.SPARQLQueryEngine
import utils.Ops.MonadInstances._
import utils.Ops._

import scala.concurrent.duration._
import scala.language.{higherKinds, postfixOps}
import scala.util.{Success, Try}

object SemanticUtils {
  def collect[A](set: Set[Try[Option[A]]]): Try[Set[A]] = {
    import utils.Ops.MonadInstances.tryM
    import scalaz.syntax.applicative._

    set.foldLeft(Try(Set.empty[A])) { (t_set, t) =>
      (t_set |@| t) {
        case (iset, Some(item)) => iset + item
        case (iset, _) => iset
      }
    }
  }

  def vectorise[A](result: RepositoryResult[A]): Vector[A] = {
    import scala.collection.JavaConverters._
    Iterations.asList(result).asScala.toVector
  }

  def statements(subject: Sesame#URI)(implicit connection: RepositoryConnection): Vector[Statement] = {
    vectorise(connection.getStatements(subject, null, null, false))
  }

  def subjects(predicate: Sesame#URI, obj: Sesame#Node)(implicit connection: RepositoryConnection): Vector[Sesame#Node] = {
    vectorise(connection.getStatements(null, predicate, obj, false)) map (_.getSubject)
  }

  def predicates(subject: Sesame#URI)(implicit connection: RepositoryConnection): Vector[Sesame#URI] = {
    vectorise(connection.getStatements(subject, null, null, false)) map (_.getPredicate)
  }

  def objects(subject: Sesame#URI, predicate: Sesame#URI)(implicit connection: RepositoryConnection): Vector[Sesame#Node] = {
    vectorise(connection.getStatements(subject, predicate, null, false)) map (_.getObject)
  }

  def objects(subject: Sesame#URI)(implicit connection: RepositoryConnection): Vector[Sesame#Node] = {
    vectorise(connection.getStatements(subject, null, null, false)) map (_.getObject)
  }
}

trait SemanticRepository extends RDFModule with RDFOpsModule {

  def rdfOps: RDFOps[Rdf] = ops

  def ns: Rdf#URI

  def addMany[T <: UniqueEntity](entities: TraversableOnce[T])(implicit serialiser: ToPG[Rdf, T]): Try[Set[PointedGraph[Rdf]]]

  def add[T <: UniqueEntity](entity: T)(implicit serialiser: ToPG[Rdf, T]): Try[PointedGraph[Rdf]]

  def update[T <: UniqueEntity, G <: UriGenerator[T]](entity: T)(implicit serialiser: ToPG[Rdf, T], idGenerator: G): Try[PointedGraph[Rdf]]

  def get[T <: UniqueEntity](implicit serialiser: FromPG[Rdf, T], compositeClassUris: CompositeClassUris[Rdf, T]): Try[Set[T]]

  def get[T <: UniqueEntity](id: String)(implicit serialiser: FromPG[Rdf, T], compositeClassUris: CompositeClassUris[Rdf, T]): Try[Option[T]]

  def getMany[T <: UniqueEntity](ids: TraversableOnce[String])(implicit serialiser: FromPG[Rdf, T], compositeClassUris: CompositeClassUris[Rdf, T]): Try[Set[T]]

  def deleteCascading(id: String): Try[Boolean]

  def deleteSimple(id: String): Try[Boolean]

  def contains(id: String): Boolean

  def close(): Unit

  private[store] def reset(): Try[Unit]
}

object SesameRepository {

  def apply(folder: Option[File], syncInterval: FiniteDuration, baseNS: Namespace) = new SesameRepository(folder, syncInterval, baseNS)

  def apply(folder: Option[File], baseNS: Namespace) = new SesameRepository(folder, baseNS = baseNS)

  def apply(syncInterval: FiniteDuration, baseNS: Namespace) = new SesameRepository(syncInterval = syncInterval, baseNS = baseNS)

  def apply(baseNS: Namespace) = new SesameRepository(baseNS = baseNS)
}

class SesameRepository(folder: Option[File] = None, syncInterval: FiniteDuration = 10.seconds, baseNS: Namespace) extends SemanticRepository with SesameModule with SPARQLQueryEngine {

  import ops._
  import SemanticUtils._

  val ns = makeUri(baseNS.base)
  implicit val namespace = baseNS

  val memStore = folder.fold {
    new MemoryStore()
  } { f =>
    val mStore = new MemoryStore(f)
    mStore.setSyncDelay(syncInterval.toMillis)
    mStore
  }

  val repo = new SailRepository(memStore)
  repo.initialize()


  // ========================================== NEW API ===================================================

  def get[T <: UniqueEntity](implicit serialiser: FromPG[Rdf, T], compositeClassUris: CompositeClassUris[Rdf, T]): Try[Set[T]] = connection { implicit connection =>
    collect {
      compositeClassUris.classes
        .flatMap(uri => subjects(rdf.`type`, uri))
        .map(subject => get[T](subject.stringValue()))
        .filter(_.isSuccess)
        .toSet
    }
  }

  def getMany[A <: UniqueEntity](uris: TraversableOnce[String])(implicit serialiser: FromPG[Rdf, A], classUris: CompositeClassUris[Rdf, A]): Try[Set[A]] = connection { implicit conn =>
    collect {
      uris
        .map(get[A](_))
        .toSet
    }
  }

  def get[A <: UniqueEntity](uri: String)(implicit serialiser: FromPG[Rdf, A], classUris: CompositeClassUris[Rdf, A]): Try[Option[A]] = connection { implicit conn =>
    graphAt(uri)
      .as[A]
      .map(Option(_))
  }


  private def graphAt[T](uri: String)(implicit compositeClassUris: CompositeClassUris[Rdf, T], connection: RepositoryConnection): PointedGraph[Rdf] = {
    val url = makeUri(uri)
    (statements _ andThen
      (_ ++
        unravel(uri, compositeClassUris.components)
          .map(s => makeUri(s.stringValue()))
          .flatMap(statements)) andThen
      rdfOps.makeGraph andThen
      (PointedGraph[Rdf](url, _))) (url)
  }

  private def unravel(uri: String, classes: Iterable[Rdf#URI])(implicit conn: RepositoryConnection): Vector[Rdf#Node] = {
    if (classes isEmpty) Vector()
    else (makeUri _ andThen objects andThen { objs =>
      val parents = objs filter (s => classes.exists(uri => conn.hasStatement(makeUri(s.stringValue()), rdf.`type`, uri, false)))
      val remaining = classes filterNot (uri => objs exists (s => conn.hasStatement(makeUri(s.stringValue()), rdf.`type`, uri, false)))

      parents ++ (parents flatMap (s => unravel(s.stringValue(), remaining)))
    }) (uri)
  }

  // =========================================================================================================

  override def addMany[T <: UniqueEntity](entities: TraversableOnce[T])(implicit serialiser: ToPG[Rdf, T]): Try[Set[PointedGraph[Rdf]]] = transaction {
    connection =>
      (entities map { entity =>
        val pointedGraph = entity.toPG
        rdfStore.appendToGraph(connection, ns, pointedGraph.graph) map (_ => pointedGraph)
      }).sequence map (_.toSet)
  }

  override def close() = {
    repo.shutDown()
  }
  override def update[T <: UniqueEntity, G <: UriGenerator[T]](entity: T)
                                                              (implicit serialiser: ToPG[Sesame, T], idGenerator: G): Try[PointedGraph[Rdf]] = transaction { connection =>
    val entityUri = idGenerator.generateUri(entity)
    for {
      _ <- deleteSimple(entityUri)
      entityGraph <- add(entity)
    } yield entityGraph
  }

  override def add[T <: UniqueEntity](entity: T)(implicit serialiser: ToPG[Rdf, T]): Try[PointedGraph[Rdf]] = transaction { connection =>
    for {
      pointedGraph <- Try(entity.toPG)
      graph = pointedGraph.graph
      _ <- rdfStore.appendToGraph(connection, ns, graph)
    } yield pointedGraph
  }

  override def deleteCascading(id: String): Try[Boolean] = delete(id)((triple, uri) => triple.getSubject == uri || triple.getObject == uri)

  override def deleteSimple(id: String): Try[Boolean] = delete(id)((triple, uri) => triple.getSubject == uri)

  def graph = connection(rdfStore.getGraph(_, ns))

  def size: Int = connection {
    rdfStore.getGraph(_, ns).map(_.size()).getOrElse(0)
  }

  override def contains(id: String): Boolean = {
    val connection = repo.getConnection
    val uri = makeUri(id)
    (for {
      graph <- rdfStore.getGraph(connection, ns)
    } yield graph.contains(uri, null, null)).getOrElse(false)
  }

  //Side-effect
  override def connection[A](f: (RepositoryConnection) => A): A = {
    val conn = repo.getConnection
    val res = f(conn)
    conn.close()
    res
  }

  private def delete(id: String)(p: (Sesame#Triple, URI) => Boolean): Try[Boolean] = transaction { connection =>
    for {
      graph <- rdfStore.getGraph(connection, ns)
      uri = makeUri(id)
      triples = graph.triples.filter(p(_, uri))
      _ <- rdfStore.removeTriples(connection, ns, triples)
    } yield true
  }

  private def transaction[A](f: RepositoryConnection => A): A = {
    val connection = repo.getConnection
    connection.begin()
    val res = f(connection)
    connection.commit()
    connection.close()
    res
  }

  override private[store] def reset(): Try[Unit] = {
    val connection = repo.getConnection
    val result = rdfStore.removeGraph(connection, ns)
    connection.close()
    result
  }
}

sealed trait ValidationResult

case class ValidationError(errors: List[String]) extends ValidationResult

case class ValidationSuccess(graph: Sesame#Graph) extends ValidationResult

