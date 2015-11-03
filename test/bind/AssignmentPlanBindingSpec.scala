package bind

import base.SesameDbSpec
import models._
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import store.Namespace
import store.bind.Bindings

import scala.util.{Failure, Success}

class AssignmentPlanBindingSpec extends SesameDbSpec {

  import ops._
  implicit val ns = Namespace("http://lwm.gm.fh-koeln.de/")

  val bindings = Bindings[Sesame](ns)
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

  val assignmentEntry = AssignmentEntry(0, Set(mandatoryT, optionalT))

  val assignmentPlanGraph = (
    URI(AssignmentPlan.generateUri(assignmentPlan)).a(lwm.AssignmentPlan)
      -- lwm.numberOfEntries ->- assignmentPlan.numberOfEntries
      -- lwm.entries ->- assignmentPlan.entries
      -- lwm.id ->- assignmentPlan.id
    ).graph

  val assignmentEntryGraph = (
    URI(AssignmentEntry.generateUri(assignmentEntry)).a(lwm.AssignmentEntry)
      -- lwm.index ->- assignmentEntry.index
      -- lwm.types ->- assignmentEntry.types
      -- lwm.id ->- assignmentEntry.id
    ).graph

  "An AssignmentPlanBinding" should {
    "return a RDF graph representation of an assignmentPlan" in {
      val graph = assignmentPlan.toPG.graph

      graph isIsomorphicWith assignmentPlanGraph shouldBe true
    }

    "return a RDF graph representation of an assignmentEntry" in {
      val graph = assignmentEntry.toPG.graph

      graph isIsomorphicWith assignmentEntryGraph shouldBe true
    }

    "return an assignmentPlan based on a RDF representation" in {
      val expectedAssignmentPlan = PointedGraph[Rdf](URI(AssignmentPlan.generateUri(assignmentPlan)), assignmentPlanGraph).as[AssignmentPlan]

      expectedAssignmentPlan match {
        case Success(s) =>
          s shouldEqual assignmentPlan
        case Failure(e) =>
          fail(s"Unable to deserialise assignmentPlan graph: $e")
      }
    }

    "return an assignmentEntry based on a RDF representation" in {
      val expectedAssignmentEntry = PointedGraph[Rdf](URI(AssignmentEntry.generateUri(assignmentEntry)), assignmentEntryGraph).as[AssignmentEntry]

      expectedAssignmentEntry match {
        case Success(s) =>
          s shouldEqual assignmentEntry
        case Failure(e) =>
          fail(s"Unable to deserialise assignmentEntry graph: $e")
      }
    }
  }



}
