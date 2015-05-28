package bind

import base.SesameDbSpec
import models.Labwork
import store.Namespace
import store.bind.Bindings
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame

import scala.util.{Failure, Success}

class LabworkBindingSpec extends SesameDbSpec{
  import ops._
  implicit val ns = Namespace("http://lwm.gm.fh-koeln.de/")

  val bindings = Bindings[Sesame](ns)
  import bindings.LabworkBinding._
  import bindings.uuidBinder

  val labwork = Labwork("AP Praktikum")
  val labworkGraph = (
      URI(Labwork.generateUri(labwork)).a(lwm.Labwork)
        -- lwm.label ->- labwork.label
        -- lwm.id->- labwork.id
    ).graph

  "A LabworkBinding" should {
    "return a RDF graph representation of a labwork" in {
      val graph = labwork.toPG.graph

      graph isIsomorphicWith labworkGraph shouldBe true
    }

    "return a labwork based on a RDF representation" in {
      val expectedLabwork = PointedGraph[Rdf](URI(Labwork.generateUri(labwork)), labworkGraph).as[Labwork]

      expectedLabwork match {
        case Success(s) =>
          s shouldEqual labwork
        case Failure(e) =>
          fail(s"Unable to deserialise labwork graph: $e")
      }
    }
  }
}
