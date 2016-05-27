package store

import java.io.File

import info.aduna.iteration.Iterations
import utils.Ops._
import utils.Ops.MonadInstances._
import models.{UniqueEntity, UriGenerator}
import org.openrdf.model.{Statement, URI}
import org.openrdf.repository.RepositoryConnection
import org.openrdf.repository.sail.SailRepository
import org.openrdf.sail.memory.MemoryStore
import org.w3.banana._
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame._
import store.sparql.SPARQLQueryEngine

import scala.concurrent.duration._
import scala.language.{higherKinds, postfixOps}
import scala.util.{Success, Try}

trait SemanticRepository extends RDFModule with RDFOpsModule {

  def rdfOps: RDFOps[Rdf] = ops

  def ns: Rdf#URI

  def addMany[T <: UniqueEntity](entities: TraversableOnce[T])(implicit serialiser: ToPG[Rdf, T]): Try[Set[PointedGraph[Rdf]]]

  def add[T <: UniqueEntity](entity: T)(implicit serialiser: ToPG[Rdf, T]): Try[PointedGraph[Rdf]]

  def update[T <: UniqueEntity, G <: UriGenerator[T]](entity: T)(implicit serialiser: ToPG[Rdf, T], idGenerator: G): Try[PointedGraph[Rdf]]

  def get[T <: UniqueEntity](implicit serialiser: FromPG[Rdf, T], classUri: ClassUrisFor[Rdf, T]): Try[Set[T]]

  def get[T <: UniqueEntity](id: String)(implicit serialiser: FromPG[Rdf, T]): Try[Option[T]]

  def getMany[T <: UniqueEntity](ids: TraversableOnce[String])(implicit serialiser: FromPG[Rdf, T]): Try[Set[T]]

  def deleteCascading(id: String): Try[Boolean]

  def deleteSimple(id: String): Try[Boolean]

  def contains(id: String): Boolean

  def close(): Unit

  private[store] def reset(): Try[Unit]
}

object  SesameRepository {

  def apply(folder: Option[File], syncInterval: FiniteDuration, baseNS: Namespace) = new SesameRepository(folder, syncInterval, baseNS)

  def apply(folder: Option[File], baseNS: Namespace) = new SesameRepository(folder, baseNS = baseNS)

  def apply(syncInterval: FiniteDuration, baseNS: Namespace) = new SesameRepository(syncInterval = syncInterval, baseNS = baseNS)

  def apply(baseNS: Namespace) = new SesameRepository(baseNS = baseNS)
}

class SesameRepository(folder: Option[File] = None, syncInterval: FiniteDuration = 10.seconds, baseNS: Namespace) extends SemanticRepository with SesameModule with SPARQLQueryEngine {

  import ops._

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


  override def addMany[T <: UniqueEntity](entities: TraversableOnce[T])(implicit serialiser: ToPG[Rdf, T]): Try[Set[PointedGraph[Rdf]]] = commited {
    connection =>
      (entities map { entity =>
        val pointedGraph = entity.toPG
        rdfStore.appendToGraph(connection, ns, pointedGraph.graph) map (_ => pointedGraph)
      }).sequence map (_.toSet)
  }

  def getMany[T <: UniqueEntity](ids: TraversableOnce[String])(implicit serialiser: FromPG[Rdf, T]): Try[Set[T]] = connection { implicit conn =>
    many[T](ids)(explicit[T])
  }

  def getManyExpanded[T <: UniqueEntity](ids: TraversableOnce[String])(implicit serialiser: FromPG[Rdf, T]): Try[Set[T]] = connection { implicit conn =>
    many[T](ids)(expanded[T](_))
  }

  def getManyExpanded[T <: UniqueEntity](ids: TraversableOnce[String], preds: URI*)(implicit serialiser: FromPG[Rdf, T]): Try[Set[T]] = connection { implicit conn =>
    many[T](ids)(uri => expanded[T](uri, preds: _*))
  }

  private def many[T <: UniqueEntity](ids: TraversableOnce[String])(f: String => Try[Option[T]])(implicit conn: RepositoryConnection, serialiser: FromPG[Rdf, T]): Try[Set[T]] = {
    import utils.Ops.MonadInstances.tryM
    import scalaz.syntax.applicative._

    ids.toVector.foldLeft(Try(Set.empty[T])) { (tset, uri) =>
      (tset |@| f(uri)) {
        case (set, Some(t)) => set + t
        case (set, _) => set
      }
    }
  }

  override def close() = {
    repo.shutDown()
  }

  override def get[T <: UniqueEntity](implicit serialiser: FromPG[Rdf, T], classUri: ClassUrisFor[Rdf, T]): Try[Set[T]] = connection {
    connection =>
      rdfStore.getGraph(connection, ns) flatMap { graph =>
        (classUri.classes flatMap {
          graph.getAllInstancesOf(_) map (_.as[T])
        } filter(_.isSuccess)).sequence map (_ toSet)
      }
  }

  override def get[T <: UniqueEntity](uri: String)(implicit serialiser: FromPG[Rdf, T]): Try[Option[T]] = connection { implicit conn =>
    explicit[T](uri)
  }

   def getExpanded[T <: UniqueEntity](uri: String, preds: URI*)(implicit serialiser: FromPG[Rdf, T]): Try[Option[T]] = connection { implicit conn =>
    expanded[T](uri, preds: _*)
  }

  private def statements(uri: URI)(implicit connection: RepositoryConnection): Vector[Statement] = {
    import scala.collection.JavaConverters._
    Iterations.asList(connection.getStatements(uri, null, null, false)).asScala.toVector
  }

  private def expand(vs: Vector[Statement], preds: URI*)(implicit connection: RepositoryConnection): Vector[Statement] = {
    val additional = {
      if (preds.isEmpty)
        vs filter (_.getObject.isURI)
      else
        (preds flatMap { pred =>
        vs filter (_.getPredicate == pred)
      }).toVector
    }
    additional.flatMap(s => statements(makeUri(s.getObject.stringValue()))) ++ vs
  }

  private def expanded[T](uri: String, preds: URI*)(implicit connection: RepositoryConnection, serialiser: FromPG[Rdf, T]): Try[Option[T]] = {
    val url = makeUri(uri)
    val vs = statements(url)
    val expanded = expand(vs, preds: _*)
    if(vs.isEmpty) Success(None)
    else PointedGraph[Rdf](url, rdfOps.makeGraph(expanded)).as[T] map (Some(_))
  }

  private def explicit[T](uri: String)(implicit connection: RepositoryConnection, serialiser: FromPG[Rdf, T]): Try[Option[T]] = {
    val url = makeUri(uri)
    val vs = statements(url)
    if(vs.isEmpty) Success(None)
    else PointedGraph[Rdf](url, rdfOps.makeGraph(vs)).as[T] map (Some(_))
  }

  def graph = connection(rdfStore.getGraph(_, ns))

  def size: Int = connection {
    rdfStore.getGraph(_, ns).map(_.size()).getOrElse(0)
  }

  override def update[T <: UniqueEntity, G <: UriGenerator[T]](entity: T)
                                                              (implicit serialiser: ToPG[Sesame, T], idGenerator: G): Try[PointedGraph[Rdf]] = commited { connection =>
    val entityUri = idGenerator.generateUri(entity)
    for {
      _ <- deleteSimple(entityUri)
      entityGraph <- add(entity)
    } yield entityGraph
  }

  override def add[T <: UniqueEntity](entity: T)(implicit serialiser: ToPG[Rdf, T]): Try[PointedGraph[Rdf]] = commited { connection =>
    for {
      pointedGraph <- Try(entity.toPG)
      graph = pointedGraph.graph
      _ <- rdfStore.appendToGraph(connection, ns, graph)
    } yield pointedGraph
  }

  override def deleteCascading(id: String): Try[Boolean] = delete(id)((triple, uri) => triple.getSubject == uri || triple.getObject == uri)

  override def deleteSimple(id: String): Try[Boolean] = delete(id)((triple, uri) => triple.getSubject == uri)

  private def delete(id: String)(p: (Sesame#Triple, URI) => Boolean): Try[Boolean] = commited { connection =>
    for {
      graph <- rdfStore.getGraph(connection, ns)
      uri = makeUri(id)
      triples = graph.triples.filter(p(_, uri))
      _ <- rdfStore.removeTriples(connection, ns, triples)
    } yield true
  }

  private def commited[A](f: RepositoryConnection => A): A = {
    val connection = repo.getConnection
    val res = f(connection)
    connection.commit()
    connection.close()
    res
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

