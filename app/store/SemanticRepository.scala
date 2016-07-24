package store

import java.io.File

import info.aduna.iteration.Iterations
import models.{UniqueEntity, UriGenerator}
import org.joda.time.DateTime
import org.openrdf.model.{Statement, URI, Value}
import org.openrdf.repository.sail.SailRepository
import org.openrdf.repository.{RepositoryConnection, RepositoryResult}
import org.openrdf.sail.memory.MemoryStore
import org.w3.banana.sesame._
import org.w3.banana.{PointedGraph, _}
import store.Prefixes.LWMPrefix
import store.bind.Bindings
import store.bind.Descriptor.{Descriptor, Ref}
import store.sparql.SPARQLQueryEngine
import utils.Ops._
import utils.Ops.MonadInstances._

import scala.concurrent.duration._
import scala.language.{higherKinds, postfixOps}
import scala.util.{Success, Try}
import utils.Ops.TraverseInstances.travO

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

  def apply(folder: Option[File], syncInterval: FiniteDuration, baseNS: Namespace) = new SesameRepository(folder, syncInterval)(baseNS)

  def apply(folder: Option[File], baseNS: Namespace) = new SesameRepository(folder)(baseNS)

  def apply(syncInterval: FiniteDuration, baseNS: Namespace) = new SesameRepository(syncInterval = syncInterval)(baseNS)

  def apply(baseNS: Namespace) = new SesameRepository()(baseNS)
}

class SesameRepository(folder: Option[File] = None, syncInterval: FiniteDuration = 10.seconds)(implicit val namespace: Namespace) extends SemanticRepository with SesameModule with SPARQLQueryEngine {

  import SemanticUtils._
  import ops._

  implicit val lwm = LWMPrefix[Rdf]
  val ns = makeUri(namespace.base)

  val memStore = folder.fold {
    new MemoryStore()
  } { f =>
    val mStore = new MemoryStore(f)
    mStore.setSyncDelay(syncInterval.toMillis)
    mStore
  }

  val repo = new SailRepository(memStore)
  repo.initialize()

  val graph = new Graph[Rdf, RepositoryConnection] {

    override val anyURI: Rdf#URI = ANY

    override val anyNode: Rdf#Node = ANY

    override def triples(u: Rdf#URI)(implicit conn: RepositoryConnection): Vector[Statement] = vectorise(conn.getStatements(u, anyURI, anyNode, false))

    override def subjects(p: Rdf#URI, o: Rdf#Node)(implicit conn: RepositoryConnection): Vector[URI] = vectorise(conn.getStatements(anyURI, p, o, false)) map (r => asUri(r.getSubject))

    override def contains(s: Rdf#URI, p: Rdf#URI, o: Rdf#Node)(implicit conn: RepositoryConnection): Boolean = conn.hasStatement(s, p, o, false)

    override def asUri(node: Rdf#Node): Rdf#URI = ops.makeUri(node.stringValue())
  }

  override def add[T <: UniqueEntity](entity: T)(implicit descriptor: Descriptor[Rdf, T]): Try[PointedGraph[Rdf]] = transact { implicit conn => insert[T](entity) }

  override def delete[T <: UniqueEntity](uri: String)(implicit descriptor: Descriptor[Rdf, T]): Try[Unit] = transact { implicit conn => remove[T](uri) }

  override def addMany[T <: UniqueEntity](entities: TraversableOnce[T])(implicit descriptor: Descriptor[Rdf, T]): Try[Set[PointedGraph[Rdf]]] = transact {
    implicit conn =>
      entities
        .map(insert[T])
        .sequence
        .map(_.toSet)
  }

  override def update[T <: UniqueEntity, G <: UriGenerator[T]](entity: T)(implicit descriptor: Descriptor[Rdf, T], idGenerator: G): Try[PointedGraph[Rdf]] = transact {
    implicit conn =>
      val entityUri = idGenerator.generateUri(entity)
      for {
        _ <- remove[T](entityUri)
        entityGraph <- insert[T](entity)
      } yield entityGraph
  }

  override def get[T <: UniqueEntity](uri: String)(implicit descriptor: Descriptor[Rdf, T]): Try[Option[T]] = connect { implicit conn => valids[T](makeUri(uri)) }

  override def getAll[T <: UniqueEntity](implicit descriptor: Descriptor[Rdf, T]): Try[Set[T]] = connect { implicit conn =>
    collect {
      graph.allOfType(descriptor.references.root)
        .map(valids[T])
        .filter(_.isSuccess)
        .toSet
    }
  }

  override def getMany[T <: UniqueEntity](uris: TraversableOnce[String])(implicit descriptor: Descriptor[Rdf, T]): Try[Set[T]] = connect { implicit conn =>
    collect {
      uris
        .map(s => valids[T](makeUri(s)))
        .filter(_.isSuccess)
        .toSet
    }
  }

  def deepGet[T <: UniqueEntity](uri: String)(implicit descriptor: Descriptor[Rdf, T]): Try[Option[T]] = connect { implicit conn => all[T](makeUri(uri)) }

  def deepGetAll[T <: UniqueEntity](implicit descriptor: Descriptor[Rdf, T]): Try[Set[T]] = connect { implicit conn =>
    collect {
      graph.allOfType(descriptor.references.root)
        .map(all[T])
        .filter(_.isSuccess)
        .toSet
    }
  }

  def deepGetMany[T <: UniqueEntity](uris: TraversableOnce[String])(implicit descriptor: Descriptor[Rdf, T]): Try[Set[T]] = connect { implicit conn =>
    collect {
      uris
        .map(s => all[T](makeUri(s)))
        .filter(_.isSuccess)
        .toSet
    }
  }

  def invalidate[T <: UniqueEntity](uri: String)(implicit descriptor: Descriptor[Rdf, T]): Try[Unit] = transact { implicit conn =>
    implicit val branching = descriptor.branching
    val bindings = Bindings[Rdf](namespace)
    Try {
      graph.validNodes(makeUri(uri))
        .foreach { node =>
          val pointer = bindings.dateTimeBinder.toPG(DateTime.now).pointer
          conn.add(node, lwm.invalidated, pointer, ns)
        }
    }
  }

  override def contains(id: String): Boolean = connect { implicit conn => has(makeUri(id)) }

  def size: Int = connect(_.size().toInt)

  override def close() = {
    repo.shutDown()
  }

  override def connect[A](f: (RepositoryConnection) => A): A = {
    val conn = repo.getConnection
    val res = f(conn)
    conn.close()
    res
  }

  private def transact[A](f: RepositoryConnection => A): A = {
    val connection = repo.getConnection
    connection.begin()
    val res = f(connection)
    connection.commit()
    connection.close()
    res
  }

  private def has(uri: Rdf#URI)(implicit conn: RepositoryConnection): Boolean = graph containsURI uri

  private def remove[T <: UniqueEntity](uri: String)(implicit descriptor: Descriptor[Rdf, T], conn: RepositoryConnection): Try[Unit] = {
    implicit val types = descriptor.references
    implicit val lwm = LWMPrefix[Rdf]
    Try {
      graph.validSubgraph(makeUri(uri))
        .fold(()) { pointed =>
          pointed
            .graph
            .triples
            .foreach(triple => conn.remove(triple.getSubject, triple.getPredicate, triple.getObject))
        }
    }
  }

  private def valids[T <: UniqueEntity](uri: Rdf#URI)(implicit descriptor: Descriptor[Rdf, T], conn: RepositoryConnection): Try[Option[T]] = {
    implicit val refs = descriptor.references
    if (has(uri)) {
      graph.validSubgraph(uri)
        .map(_.as[T](descriptor.binder))
        .sequenceM
    } else Success(None)
  }

  private def all[T <: UniqueEntity](uri: Rdf#URI)(implicit descriptor: Descriptor[Rdf, T], conn: RepositoryConnection): Try[Option[T]] = {
    implicit val refs = descriptor.references
    if (has(uri)) {
      graph.subgraph(uri)
        .map(_.as[T](descriptor.binder))
        .sequenceM
    } else Success(None)
  }

  private def insert[T <: UniqueEntity](entity: T)(implicit descriptor: Descriptor[Rdf, T], conn: RepositoryConnection): Try[PointedGraph[Rdf]] = {
    for {
      pointed <- Try(entity.toPG(descriptor.binder))
      _ <- rdfStore appendToGraph(conn, ns, pointed.graph)
    } yield pointed
  }

  override private[store] def reset(): Try[Unit] = transact {
    conn =>
      rdfStore removeGraph(conn, ns)
  }
}

abstract class Graph[Rdf <: RDF, Connection](implicit ops: RDFOps[Rdf]) {

  import ops._

  lazy val rdf = RDFPrefix[Rdf]
  lazy val lwm = LWMPrefix[Rdf]

  def anyNode: Rdf#Node
  def anyURI: Rdf#URI

  def subjects(p: Rdf#URI, o: Rdf#Node)(implicit conn: Connection): Vector[Rdf#URI]

  def asUri(node: Rdf#Node): Rdf#URI

  def contains(s: Rdf#URI, p: Rdf#URI, o: Rdf#Node)(implicit conn: Connection): Boolean

  def triples(u: Rdf#URI)(implicit conn: Connection): Vector[Rdf#Triple]

  def triplesWhere(u: Rdf#URI)(p: Rdf#Node => Boolean)(implicit conn: Connection): Vector[Rdf#Triple] = triples(u) filter { triple =>
    val (_, _, o) = ops.fromTriple(triple)
    p(o)
  }

  def containsURI(s: Rdf#URI)(implicit conn: Connection): Boolean = contains(s, anyURI, anyNode)

  def subjects(o: Rdf#Node)(implicit conn: Connection): Vector[Rdf#URI] = subjects(anyURI, o)

  def objects(s: Rdf#URI)(implicit conn: Connection): Vector[Rdf#Node] = triples(s) map (triple => ops.fromTriple(triple)._3)

  def hasProperty(s: Rdf#URI, p: Rdf#URI)(implicit conn: Connection): Boolean = contains(s, p, anyNode)

  def asNode(uri: Rdf#URI): Rdf#Node = uri

  def branches(t: Rdf#URI)(implicit conn: Connection): Vector[Rdf#URI] = objects(t) filter (_.isURI) map asUri

  def pointed(u: Rdf#URI): Iterable[Rdf#Triple] => PointedGraph[Rdf] = ops.makeGraph _ andThen (PointedGraph[Rdf](u, _))

  def allOfType(typeTag: Rdf#URI)(implicit conn: Connection): Vector[Rdf#URI] = subjects(rdf.`type`, typeTag)

  def isOfType(node: Rdf#URI, typeTag: Rdf#URI)(implicit conn: Connection): Boolean = contains(node, rdf.`type`, typeTag)

  def validSubgraph(uri: Rdf#URI)(implicit refs: Ref[Rdf#URI], conn: Connection): Option[PointedGraph[Rdf]] = induce(uri)(n => !hasProperty(n, lwm.invalidated))

  def invalidSubgraph(uri: Rdf#URI)(implicit refs: Ref[Rdf#URI], conn: Connection): Option[PointedGraph[Rdf]] = induce(uri)(n => hasProperty(n, lwm.invalidated))

  def subgraph(uri: Rdf#URI)(implicit refs: Ref[Rdf#URI], conn: Connection): Option[PointedGraph[Rdf]] = induce(uri)(_ => true)

  def validNodes(uri: Rdf#URI)(implicit refs: Ref[Rdf#URI], conn: Connection): List[Rdf#URI] = unfold(uri)(n => !hasProperty(n, lwm.invalidated))

  def induce(uri: Rdf#URI)(p: Rdf#URI => Boolean)(implicit refs: Ref[Rdf#URI], conn: Connection): Option[PointedGraph[Rdf]] = {
    val objs = memoize(branches)
    if (p(uri))
      Some(pointed(uri) {
        refs
          .leftDeref(uri) { (root, ref) => objs(root).filter(node => isOfType(node, ref)).toList }
          .filter(p)
          .flatMap(triplesWhere(_)(node => p(asUri(node))))
      })
    else None
  }

  def deduce(uri: Rdf#URI)(p: Rdf#URI => Boolean)(implicit refs: Ref[Rdf#URI], conn: Connection): Option[PointedGraph[Rdf]] = {
    val subjs = memoize(subjects)
    if (p(uri)) {
      Some(pointed(uri) {
        refs
          .rightDeref(uri) { (root, ref) => subjs(root).filter(node => isOfType(node, ref)).toList }
          .filter(p)
          .flatMap(triples)
      })
    } else None
  }

  def unfold(uri: Rdf#URI)(p: Rdf#URI => Boolean)(implicit refs: Ref[Rdf#URI], conn: Connection): List[Rdf#URI] = {
    val objs = memoize(branches)
    val subjs = memoize(subjects)

    refs
      .deref(uri) { (root, ref) => objs(root).filter(node => isOfType(node, ref)).toList } { (root, ref) => subjs(root).filter(node => isOfType(node, ref)).toList }
      .filter(p)
  }

  private def memoize(f: Rdf#URI => Vector[Rdf#URI]) = {
    val store = scala.collection.mutable.Map[Rdf#URI, Vector[Rdf#URI]]()
    (key: Rdf#URI) =>
      if (store contains key) store(key)
      else {
        val value = f(key)
        store + (key -> value)
        value
      }
  }
}