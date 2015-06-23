package store

import java.io.File

import models.{UniqueEntity, UriGenerator}
import org.openrdf.model.Model
import org.openrdf.repository.sail.SailRepository
import org.openrdf.sail.memory.MemoryStore
import org.w3.banana._
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.{Sesame, SesameModule}

import scala.concurrent.duration._
import scala.util.Try


trait SemanticRepository extends RDFModule with RDFOpsModule {

  def rdfOps: RDFOps[Rdf] = ops

  def ns: Rdf#URI

  def add[T <: UniqueEntity](entity: T)(implicit serialiser: ToPG[Rdf, T]): Try[Rdf#Graph]

  def update[T <: UniqueEntity, G <: UriGenerator[T]](entity: T)(implicit serialiser: ToPG[Rdf, T], idGenerator: G): Try[Rdf#Graph]

  def get[T <: UniqueEntity](implicit serialiser: FromPG[Rdf, T], classUri: ClassUrisFor[Rdf, T]): Try[Set[T]]

  def get[T <: UniqueEntity](id: String)(implicit serialiser: FromPG[Rdf, T]): Try[Option[T]]

  def delete(id: String): Try[Rdf#Graph]

  def contains(id: String): Boolean

  private[store] def reset(): Try[Unit]

  def close(): Unit
}


class SesameRepository(folder: Option[File] = None, syncInterval: FiniteDuration = 10.seconds, baseNS: Namespace) extends SemanticRepository with SesameModule {

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


  override def add[T <: UniqueEntity](entity: T)(implicit serialiser: ToPG[Rdf, T]): Try[Rdf#Graph] = {
    val connection = repo.getConnection
    val g = entity.toPG.graph
    rdfStore.appendToGraph(connection, ns, g)

    connection.commit()
    connection.close()
    Try(g)
  }


  override def close() = {
    repo.shutDown()
  }

  override def get[T <: UniqueEntity](implicit serialiser: FromPG[Rdf, T], classUri: ClassUrisFor[Rdf, T]): Try[Set[T]] = {
    val connection = repo.getConnection
    val ts = rdfStore.getGraph(connection, ns).map { graph =>
      classUri.classes.toList.flatMap { clazz =>
        graph.getAllInstancesOf(clazz).toList.map(_.as[T])
      }.filter(_.isSuccess).map(_.get).toSet
    }

    connection.close()

    ts
  }

  override def get[T <: UniqueEntity](id: String)(implicit serialiser: FromPG[Rdf, T]): Try[Option[T]] = {
    val connection = repo.getConnection
    val uri = makeUri(id)

    val ts = rdfStore.getGraph(connection, ns).map { graph =>
      PointedGraph[Rdf](uri, graph).as[T].toOption
    }

    connection.close()

    ts
  }

  def graph = {
    val connection = repo.getConnection
    val g = rdfStore.getGraph(connection, ns)
    connection.close()
    g
  }

  override def delete(id: String): Try[Rdf#Graph] = {
    val connection = repo.getConnection
    val uri = makeUri(id)

    val g = rdfStore.getGraph(connection, ns).map { graph =>
      rdfStore.removeTriples(connection, ns, graph.triples.filter(t => t.getSubject == uri || t.getObject == uri))
      graph
    }

    connection.commit()
    connection.close()

    g
  }

  def size: Int = {
    val connection = repo.getConnection
    val s = rdfStore.getGraph(connection, ns).map(_.size()).getOrElse(0)
    connection.close()
    s
  }

  override def update[T <: UniqueEntity, G <: UriGenerator[T]](entity: T)(implicit serialiser: ToPG[Sesame, T], idGenerator: G): Try[Model] = {
    val connection = repo.getConnection
    val entityUri = idGenerator.generateUri(entity)

    val result = (for {
      graph <- delete(entityUri)
    } yield add(entity)).flatten

    connection.close()

    result
  }

  override private[store] def reset(): Try[Unit] = {
    val connection = repo.getConnection
    val result = rdfStore.removeGraph(connection, ns)
    connection.close()
    result
  }

  override def contains(id: String): Boolean = {
    val connection = repo.getConnection
    val uri = makeUri(id)
    (for {
      graph <- rdfStore.getGraph(connection, ns)
    } yield graph.contains(uri, null, null)).getOrElse(false)
  }
}


object SesameRepository {

  def apply(folder: File, syncInterval: FiniteDuration, baseNS: Namespace) = new SesameRepository(Some(folder), syncInterval, baseNS)

  def apply(folder: File, baseNS: Namespace) = new SesameRepository(Some(folder), baseNS = baseNS)

  def apply(syncInterval: FiniteDuration, baseNS: Namespace) = new SesameRepository(syncInterval = syncInterval, baseNS = baseNS)

  def apply(baseNS: Namespace) = new SesameRepository(baseNS = baseNS)
}

sealed trait ValidationResult

case class ValidationError(errors: List[String]) extends ValidationResult

case class ValidationSuccess(graph: Sesame#Graph) extends ValidationResult

