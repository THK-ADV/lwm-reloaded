package store

import org.w3.banana.{RDF, RDFOps}

trait RepositorySerialiser[T, Rdf <: RDF, Uri <: Rdf#URI, Graph <: Rdf#Graph] {
  def typeURI(implicit ops: RDFOps[Rdf]): Uri

  def serialise(entity: T)(implicit ops: RDFOps[Rdf]): (Uri, Graph)

  def deserialise(graph: Graph)(implicit ops: RDFOps[Rdf]): T
}


