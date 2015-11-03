package bind

import base.SesameDbSpec
import models._
import store.Namespace
import store.bind.Bindings
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame

import scala.util.{Failure, Success}

class LabworkBindingSpec extends SesameDbSpec {

  import ops._

  implicit val ns = Namespace("http://lwm.gm.fh-koeln.de/")

  val bindings = Bindings[Sesame](ns)

  import bindings.LabworkBinding._
  import bindings.AssignmentPlanBinding._
  import bindings.AssignmentEntryBinding._
  import bindings.uuidBinder
  import bindings.entryTypeBinder

  val mandatoryT = EntryType("Mandatory")
  val optionalT = EntryType("Optional")

  val assignmentPlan = AssignmentPlan(2, Set(
    AssignmentEntry(0, Set(mandatoryT, optionalT)),
    AssignmentEntry(1, Set(optionalT))
  ))

  val labwork = Labwork("AP Praktikum", "AP Praktikum", Semester.randomUUID, Course.randomUUID, Degree.randomUUID, assignmentPlan, Labwork.randomUUID)
  val labworkGraph = (
    URI(Labwork.generateUri(labwork)).a(lwm.Labwork)
      -- lwm.label ->- labwork.label
      -- lwm.description ->- labwork.description
      -- lwm.semester ->- labwork.semester
      -- lwm.course ->- labwork.course
      -- lwm.degree ->- labwork.degree
      -- lwm.assignmentPlan ->- labwork.assignmentPlan
      -- lwm.id ->- labwork.id
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
