package store

import java.io.File
import info.aduna.iteration.Iterations
import models.{UniqueEntity, UriGenerator}
import org.openrdf.model.{Statement, URI}
import org.openrdf.repository.sail.SailRepository
import org.openrdf.repository.{RepositoryConnection, RepositoryResult}
import org.openrdf.sail.memory.MemoryStore
import org.w3.banana.sesame._
import org.w3.banana.{PointedGraph, _}
import store.bind.Descriptor.{CompositeClassUris, Descriptor}
import store.sparql.SPARQLQueryEngine
import utils.Ops._
import utils.Ops.MonadInstances._
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

  def add[T <: UniqueEntity](entity: T)(implicit descriptor: Descriptor[Rdf, T]): Try[PointedGraph[Rdf]]

  def addMany[T <: UniqueEntity](entities: TraversableOnce[T])(implicit descriptor: Descriptor[Rdf, T]): Try[Set[PointedGraph[Rdf]]]

  def update[T <: UniqueEntity, G <: UriGenerator[T]](entity: T)(implicit descriptor: Descriptor[Rdf, T], idGenerator: G): Try[PointedGraph[Rdf]]

  def getAll[T <: UniqueEntity](implicit descriptor: Descriptor[Rdf, T]): Try[Set[T]]

  def get[T <: UniqueEntity](id: String)(implicit descriptor: Descriptor[Rdf, T]): Try[Option[T]]

  def getMany[T <: UniqueEntity](ids: TraversableOnce[String])(implicit descriptor: Descriptor[Rdf, T]): Try[Set[T]]

  def delete[T <: UniqueEntity](uri: String)(implicit descriptor: Descriptor[Rdf, T]): Try[Unit]

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

  import SemanticUtils._
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

  override def add[T <: UniqueEntity](entity: T)(implicit descriptor: Descriptor[Rdf, T]): Try[PointedGraph[Rdf]] = transact { implicit conn => add0[T](entity) }

  override def addMany[T <: UniqueEntity](entities: TraversableOnce[T])(implicit descriptor: Descriptor[Rdf, T]): Try[Set[PointedGraph[Rdf]]] = transact {
    implicit conn =>
      entities
        .map(add0[T])
        .sequence
        .map(_.toSet)
  }

  override def update[T <: UniqueEntity, G <: UriGenerator[T]](entity: T)(implicit descriptor: Descriptor[Rdf, T], idGenerator: G): Try[PointedGraph[Rdf]] = transact {
    implicit conn =>
      val entityUri = idGenerator.generateUri(entity)
      for {
        _ <- delete[T](entityUri)
        entityGraph <- add0[T](entity)
      } yield entityGraph
  }

  override def delete[T <: UniqueEntity](uri: String)(implicit descriptor: Descriptor[Rdf, T]): Try[Unit] = transact {
    implicit conn =>
      implicit val compositeClassUris = descriptor.compositeClassUris
      graphAt(uri)
        .graph
        .triples
        .map(triple => Try(conn.remove(triple.getSubject, triple.getPredicate, triple.getObject)))
        .sequence
        .map(_ => ())
  }

  override def get[T <: UniqueEntity](uri: String)(implicit descriptor: Descriptor[Rdf, T]): Try[Option[T]] = connect { implicit conn => get0(uri) }

  override def getAll[T <: UniqueEntity](implicit descriptor: Descriptor[Rdf, T]): Try[Set[T]] = connect { implicit connection =>
    collect {
      descriptor.compositeClassUris.classes
        .flatMap(uri => subjects(rdf.`type`, uri))
        .map(subject => get0[T](subject.stringValue()))
        .filter(_.isSuccess)
        .toSet
    }
  }

  override def getMany[T <: UniqueEntity](uris: TraversableOnce[String])(implicit descriptor: Descriptor[Rdf, T]): Try[Set[T]] = connect { implicit conn =>
    collect {
      uris
        .map(get0[T])
        .filter(_.isSuccess)
        .toSet
    }
  }

  override def contains(id: String): Boolean = connect { conn =>
    val uri = makeUri(id)
    conn.hasStatement(uri, null, null, false)
  }

  def size: Int = connect(_.size().toInt)

  override def close() = {
    repo.shutDown()
  }

  private def add0[T <: UniqueEntity](entity: T)(implicit descriptor: Descriptor[Rdf, T], conn: RepositoryConnection): Try[PointedGraph[Rdf]] = {
    for {
      pointed <- Try(entity.toPG(descriptor.binder))
      _ <- rdfStore appendToGraph(conn, ns, pointed.graph)
    } yield pointed
  }

  private def get0[T <: UniqueEntity](uri: String)(implicit descriptor: Descriptor[Rdf, T], conn: RepositoryConnection): Try[Option[T]] = {
    if (contains(uri)) {
      implicit val compositeClassUris = descriptor.compositeClassUris
      implicit val binder = descriptor.binder
      graphAt(uri)
        .as[T]
        .map(Option(_))
    }
    else Success(None)
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
      val (nclasses, candidates, _) =
        classes.foldLeft((Vector.empty[Rdf#URI], Vector.empty[Rdf#Node], objs)) {
          case ((cls, csd, os), c) =>
            val fs = os filter (s => conn.hasStatement(makeUri(s.stringValue()), rdf.`type`, c, false))
            if (fs isEmpty) (cls :+ c, csd, os)
            else (cls, csd ++ fs, os filterNot (s => conn.hasStatement(makeUri(s.stringValue()), rdf.`type`, c, false)))
        }
      candidates ++ (candidates flatMap (a => unravel(a.stringValue(), nclasses)))
    }) (uri)
  }

  private def transact[A](f: RepositoryConnection => A): A = {
    val connection = repo.getConnection
    connection.begin()
    val res = f(connection)
    connection.commit()
    connection.close()
    res
  }

  override def connect[A](f: (RepositoryConnection) => A): A = {
    val conn = repo.getConnection
    val res = f(conn)
    conn.close()
    res
  }

  override private[store] def reset(): Try[Unit] = transact { conn =>
    rdfStore removeGraph(conn, ns)
  }
}

sealed trait ValidationResult

case class ValidationError(errors: List[String]) extends ValidationResult

case class ValidationSuccess(graph: Sesame#Graph) extends ValidationResult

