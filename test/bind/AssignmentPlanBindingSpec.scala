package bind

import base.SesameDbSpec
import models.{AssignmentPlan, AssignmentEntry, AssignmentEntryType}
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import store.Namespace
import store.bind.Bindings
import models.AssignmentEntryType._
import scala.util.{Failure, Success}

class AssignmentPlanBindingSpec extends SesameDbSpec {

  import ops._
  implicit val ns = Namespace("http://lwm.gm.fh-koeln.de/")

  val bindings = Bindings[Sesame](ns)
  import bindings.AssignmentPlanBinding._
  import bindings.AssignmentEntryBinding._
  import bindings.AssignmentEntryTypeBinding._
  import bindings.uuidBinder

  val assignmentPlan = AssignmentPlan(2, 2, Set(
    AssignmentEntry(0, "label 1", Set(Attendance, Certificate)),
    AssignmentEntry(1, "label 2", Set(Attendance, Bonus))
  ))

  val assignmentEntry = assignmentPlan.entries.head

  val assignmentPlanGraph = (
    URI(AssignmentPlan.generateUri(assignmentPlan)).a(lwm.AssignmentPlan)
      -- lwm.attendance ->- assignmentPlan.attendance
      -- lwm.mandatory ->- assignmentPlan.mandatory
      -- lwm.entries ->- assignmentPlan.entries
      -- lwm.id ->- assignmentPlan.id
    ).graph

  val assignmentEntryGraph = (
    URI(AssignmentEntry.generateUri(assignmentEntry)).a(lwm.AssignmentEntry)
      -- lwm.index ->- assignmentEntry.index
      -- lwm.label ->- assignmentEntry.label
      -- lwm.types ->- assignmentEntry.types
      -- lwm.duration ->- assignmentEntry.duration
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
