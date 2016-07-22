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

    override type AnyNode = Rdf#NodeAny

    override val anyNode: AnyNode = ANY

    override def triples(u: URI)(implicit conn: RepositoryConnection): Vector[Statement] = vectorise(conn.getStatements(u, anyNode, anyNode, false))

    override def subjects(p: URI, o: Value)(implicit conn: RepositoryConnection): Vector[URI] = vectorise(conn.getStatements(anyNode, p, o, false)) map (r => asUri(r.getSubject))

    override def contains(s: URI, p: URI, o: Value)(implicit conn: RepositoryConnection): Boolean = conn.hasStatement(s, p, o, false)

    override def asUri(node: Rdf#Node): Rdf#URI = ops.makeUri(node.stringValue())

    override def objects(s: Rdf#URI)(implicit conn: RepositoryConnection): Vector[Rdf#Node] = triples(s) map (_.getObject)

    override def subjects(o: Rdf#Node)(implicit conn: RepositoryConnection): Vector[Rdf#URI] = subjects(anyNode, o)
  }

  override def add[T <: UniqueEntity](entity: T)(implicit descriptor: Descriptor[Rdf, T]): Try[PointedGraph[Rdf]] = transact { implicit conn => add0[T](entity) }

  override def delete[T <: UniqueEntity](uri: String)(implicit descriptor: Descriptor[Rdf, T]): Try[Unit] = transact { implicit conn => delete0[T](uri) }


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
        _ <- delete0[T](entityUri)
        entityGraph <- add0[T](entity)
      } yield entityGraph
  }

  override def get[T <: UniqueEntity](uri: String)(implicit descriptor: Descriptor[Rdf, T]): Try[Option[T]] = connect { implicit conn =>
    get0[T](uri)(node => !graph.hasProperty(node, lwm.invalidated))
  }

  override def getAll[T <: UniqueEntity](implicit descriptor: Descriptor[Rdf, T]): Try[Set[T]] = connect { implicit conn =>
    collect {
      graph.subjects(rdf.`type`, descriptor.references.root)
        .map(subject => get0[T](subject.stringValue())(node => !graph.hasProperty(node, lwm.invalidated)))
        .filter(_.isSuccess)
        .toSet
    }
  }


  override def getMany[T <: UniqueEntity](uris: TraversableOnce[String])(implicit descriptor: Descriptor[Rdf, T]): Try[Set[T]] = connect { implicit conn =>
    collect {
      uris
        .map(subject => get0[T](subject)(node => !graph.hasProperty(node, lwm.invalidated)))
        .filter(_.isSuccess)
        .toSet
    }
  }

  def vget[T <: UniqueEntity](uri: String)(implicit descriptor: Descriptor[Rdf, T]): Try[Option[T]] = connect { implicit conn =>
    get0[T](uri)(_ => true)
  }

  def vgetAll[T <: UniqueEntity](implicit descriptor: Descriptor[Rdf, T]): Try[Set[T]] = connect { implicit conn =>
    collect {
      graph.subjects(rdf.`type`, descriptor.references.root)
        .map(subject => get0[T](subject.stringValue())(_ => true))
        .filter(_.isSuccess)
        .toSet
    }
  }

  def vgetMany[T <: UniqueEntity](uris: TraversableOnce[String])(implicit descriptor: Descriptor[Rdf, T]): Try[Set[T]] = connect { implicit conn =>
    collect {
      uris
        .map(subject => get0[T](subject)(_ => true))
        .filter(_.isSuccess)
        .toSet
    }
  }


  def invalidate[T <: UniqueEntity](uri: String)(implicit descriptor: Descriptor[Rdf, T]): Try[Unit] = transact { implicit conn =>
    implicit val refs = descriptor.references
    val bindings = Bindings[Rdf](namespace)
    Try {
      graph
        .unfold(makeUri(uri))(s => graph.hasProperty(s, lwm.invalidated))
        .foreach { node =>
          val url = makeUri(node.stringValue())
          val pointer = bindings.dateTimeBinder.toPG(DateTime.now).pointer
          conn.add(url, lwm.invalidated, pointer)
        }
    }
  }

  override def contains(id: String): Boolean = connect { implicit conn => contains0(id) }

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

  private def contains0(uri: String)(implicit conn: RepositoryConnection): Boolean = graph.contains(makeUri(uri), graph.anyNode, graph.anyNode)

  private def delete0[T <: UniqueEntity](uri: String)(implicit descriptor: Descriptor[Rdf, T], conn: RepositoryConnection): Try[Unit] = {
    implicit val types = descriptor.references
    implicit val lwm = LWMPrefix[Rdf]

    graph.induce(makeUri(uri))(node => !graph.hasProperty(node, lwm.invalidated))
      .fold(Try(())) { pointed =>
        pointed
          .graph
          .triples
          .map(triple => Try(conn.remove(triple.getSubject, triple.getPredicate, triple.getObject)))
          .sequence
          .map(_ => ())
      }
  }

  private def get0[T <: UniqueEntity](uri: String)(p: Rdf#URI => Boolean)(implicit descriptor: Descriptor[Rdf, T], conn: RepositoryConnection): Try[Option[T]] = {
    if (contains0(uri)) {
      implicit val refs = descriptor.references
      implicit val binder = descriptor.binder
      graph.induce(makeUri(uri))(p)
        .map(_.as[T])
        .sequenceM
    } else Success(None)
  }

  private def add0[T <: UniqueEntity](entity: T)(implicit descriptor: Descriptor[Rdf, T], conn: RepositoryConnection): Try[PointedGraph[Rdf]] = {
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

  type AnyNode <: Rdf#Node

  def anyNode: AnyNode

  def objects(s: Rdf#URI)(implicit conn: Connection): Vector[Rdf#Node]

  def subjects(p: Rdf#URI, o: Rdf#Node)(implicit conn: Connection): Vector[Rdf#URI]

  def subjects(o: Rdf#Node)(implicit conn: Connection): Vector[Rdf#URI]

  def asUri(node: Rdf#Node): Rdf#URI

  def contains(s: Rdf#URI, p: Rdf#URI, o: Rdf#Node)(implicit conn: Connection): Boolean

  def triples(u: Rdf#URI)(implicit conn: Connection): Vector[Rdf#Triple]

  def hasProperty(s: Rdf#URI, p: Rdf#URI)(implicit conn: Connection): Boolean = contains(s, p, anyNode)

  def asNode(uri: Rdf#URI): Rdf#Node = uri

  def branches(t: Rdf#URI)(implicit conn: Connection): Vector[Rdf#URI] = objects(t) filter (_.isURI) map asUri

  def pointed(u: Rdf#URI)(implicit conn: Connection): Iterable[Rdf#Triple] => PointedGraph[Rdf] = ops.makeGraph _ andThen (PointedGraph[Rdf](u, _))

  def allOfType(typeTag: Rdf#URI)(implicit conn: Connection): Vector[Rdf#URI] = subjects(rdf.`type`, typeTag)

  def isOfType(node: Rdf#URI, typeTag: Rdf#URI)(implicit conn: Connection): Boolean = contains(node, rdf.`type`, typeTag)

  def induce(uri: Rdf#URI)(p: Rdf#URI => Boolean)(implicit refs: Ref[Rdf#URI], conn: Connection): Option[PointedGraph[Rdf]] = {
    val objs = memoize[Rdf](branches)
    if (p(uri))
      Some(pointed(uri)(conn) {
        refs
          .leftDeref(uri) { (root, ref) => objs(root).filter(node => isOfType(node, ref)).toList }
          .filter(p)
          .flatMap(triples)
      })
    else None
  }

  def deduce(uri: Rdf#URI)(p: Rdf#URI => Boolean)(implicit refs: Ref[Rdf#URI], conn: Connection): Option[PointedGraph[Rdf]] = {
    val subjs = memoize[Rdf](subjects)
    if (p(uri)) {
      Some(pointed(uri)(conn) {
        refs
          .rightDeref(uri) { (root, ref) => subjs(root).filter(node => isOfType(node, ref)).toList }
          .filter(p)
          .flatMap(triples)
      })
    } else None

  }

  def unfold(uri: Rdf#URI)(p: Rdf#URI => Boolean)(implicit refs: Ref[Rdf#URI], conn: Connection): List[Rdf#URI] = {
    val objs = memoize[Rdf](branches)
    val subjs = memoize[Rdf](subjects)

    refs
      .deref(uri) { (root, ref) => objs(root).filter(node => isOfType(node, ref)).toList } { (root, ref) => subjs(root).filter(node => isOfType(node, ref)).toList }
      .filter(p)
  }

  private def memoize[Rdf <: RDF](f: Rdf#URI => Vector[Rdf#URI]) = {
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