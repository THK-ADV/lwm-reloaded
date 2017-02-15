package bind

import base.SesameDbSpec
import models.PostgresDegree$
import org.w3.banana.PointedGraph
import scala.util.{Failure, Success}

class DegreeBindingSpec extends SesameDbSpec {

  import bindings.{
  DegreeDescriptor,
  dateTimeBinder,
  uuidBinder}
  import ops._

  implicit val degreeBinder = DegreeDescriptor.binder

  val degree = PostgresDegree("degree", "abbreviation")
  val degreeGraph = (
    URI(PostgresDegree.generateUri(degree)).a(lwm.Degree)
      -- lwm.label ->- degree.label
      -- lwm.abbreviation ->- degree.abbreviation
      -- lwm.id ->- degree.id
    ).graph

  "A DegreeBindingSpec" should {

    "return a RDF graph representation of a student" in {
      val graph = degree.toPG.graph

      graph isIsomorphicWith degreeGraph shouldBe true
    }

    "return a student based on a RDF graph representation" in {
      val expecteddegree = PointedGraph[Rdf](URI(PostgresDegree.generateUri(degree)), degreeGraph).as[Degree]

      expecteddegree match {
        case Success(s) =>
          s shouldEqual degree
        case Failure(e) =>
          fail(s"Unable to deserialise degree graph: $e")
      }
    }
  }
}
