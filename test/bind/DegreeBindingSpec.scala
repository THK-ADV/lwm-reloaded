package bind

import base.SesameDbSpec
import models.Degree
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import store.Namespace
import store.bind.Bindings

import scala.util.{Failure, Success}

class DegreeBindingSpec extends SesameDbSpec {
  import ops._
  implicit val ns = Namespace("http://lwm.gm.fh-koeln.de/")

  val bindings = Bindings[Sesame](ns)
  import bindings.DegreeBinding._
  import bindings.uuidBinder

  val degree = Degree("degree", "description", Degree.randomUUID)
  val degreeGraph = (
    URI(Degree.generateUri(degree)).a(lwm.Degree)
      -- lwm.label ->- degree.label
      -- lwm.description ->- degree.description
      -- lwm.id ->- degree.id
    ).graph

  "A DegreeBindingSpec" should {
    "return a RDF graph representation of a student" in {
      val graph = degree.toPG.graph

      graph isIsomorphicWith degreeGraph shouldBe true
    }
    "return a student based on a RDF graph representation" in {
      val expecteddegree = PointedGraph[Rdf](URI(Degree.generateUri(degree)), degreeGraph).as[Degree]

      expecteddegree match {
        case Success(s) =>
          s shouldEqual degree
        case Failure(e) =>
          fail(s"Unable to deserialise degree graph: $e")
      }
    }
  }
}
