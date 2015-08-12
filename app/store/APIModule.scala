package store

import org.openrdf.repository.RepositoryConnection
import org.w3.banana._
import org.w3.banana.io._
import org.w3.banana.sesame.SesameModule

import scala.language.higherKinds
import scala.util.Try


trait APIModule extends RDFModule
  with RDFOpsModule
  with RecordBinderModule
  with SparqlGraphModule
  with RDFXMLReaderModule
  with TurtleReaderModule
  with NTriplesReaderModule
  with JsonLDReaderModule
  with RDFXMLWriterModule
  with TurtleWriterModule
  with NTriplesWriterModule
  with JsonLDWriterModule
  with JsonSolutionsWriterModule
  with XmlSolutionsWriterModule
  with JsonQueryResultsReaderModule
  with XmlQueryResultsReaderModule {
    type Rdf <: RDF
    type AFunctor[+A]

    implicit val recordBinder: binder.RecordBinder[Rdf]

    implicit val sparqlOps: SparqlOps[Rdf]

    implicit val sparqlGraph: SparqlEngine[Rdf, Try, Rdf#Graph]

    implicit val rdfStore: RDFStore[Rdf, Try, RepositoryConnection] with SparqlUpdate[Rdf, Try, RepositoryConnection]

    implicit val rdfXMLReader: RDFReader[Rdf, Try, RDFXML]

    implicit val turtleReader: RDFReader[Rdf, Try, Turtle]

    implicit val jsonldReader: RDFReader[Rdf, Try, JsonLd]

    implicit val ntriplesReader: RDFReader[Rdf, Try, NTriples]

    implicit val ntriplesWriter: RDFWriter[Rdf, Try, NTriples]

    implicit val jsonSolutionsWriter: SparqlSolutionsWriter[Rdf, SparqlAnswerJson]

    implicit val xmlSolutionsWriter: SparqlSolutionsWriter[Rdf, SparqlAnswerXml]

    implicit val jsonQueryResultsReader: SparqlQueryResultsReader[Rdf, SparqlAnswerJson]

    implicit val xmlQueryResultsReader: SparqlQueryResultsReader[Rdf, SparqlAnswerXml]
}


trait SesameAPI extends APIModule with SesameModule {
  override type AFunctor[+A] = Try[A]
}