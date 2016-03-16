package bind

import java.util.UUID

import base.SesameDbSpec
import models.{Labwork, AssignmentPlan, AssignmentEntry, AssignmentEntryType}
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
  import bindings.uuidRefBinder

  val assignmentPlan = AssignmentPlan(UUID.randomUUID(), 2, 2, Set(
    AssignmentEntry(0, "label 1", Set(Attendance, Certificate)),
    AssignmentEntry(1, "label 2", Set(Attendance, Bonus))
  ))

  val assignmentEntry = assignmentPlan.entries.head

  val assignmentPlanGraph = (
    URI(AssignmentPlan.generateUri(assignmentPlan)).a(lwm.AssignmentPlan)
      .--(lwm.labwork).->-(assignmentPlan.labwork)(ops, uuidRefBinder(Labwork.splitter))
      -- lwm.attendance ->- assignmentPlan.attendance
      -- lwm.mandatory ->- assignmentPlan.mandatory
      -- lwm.entries ->- assignmentPlan.entries
      -- lwm.id ->- assignmentPlan.id
    ).graph

  val assignmentEntryGraph = (
    URI("#").a(lwm.AssignmentEntry)
      -- lwm.index ->- assignmentEntry.index
      -- lwm.label ->- assignmentEntry.label
      -- lwm.types ->- assignmentEntry.types
      -- lwm.duration ->- assignmentEntry.duration
    ).graph

  "An AssignmentPlanBinding" should {

    "successfully serialise an assignmentPlan" in {
      val plan = assignmentPlanBinder.fromPG(assignmentPlan.toPG)

      plan shouldBe Success(assignmentPlan)
    }

    "successfully serialise an assignmentEntry" in {
      val entry = assignmentEntryBinder.fromPG(assignmentEntry.toPG)

      entry shouldBe Success(assignmentEntry)
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
      val expectedAssignmentEntry = PointedGraph[Rdf](URI("#"), assignmentEntryGraph).as[AssignmentEntry]

      expectedAssignmentEntry match {
        case Success(s) =>
          s shouldEqual assignmentEntry
        case Failure(e) =>
          fail(s"Unable to deserialise assignmentEntry graph: $e")
      }
    }
  }
}
