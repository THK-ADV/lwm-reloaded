package bind.schedule

import base.SesameDbSpec
import models.schedules.GroupScheduleAssociation
import store.Namespace
import store.bind.Bindings
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame

import scala.util.{Failure, Success}

class GroupScheduleAssociationBindingSpec extends SesameDbSpec {
  import ops._
  implicit val ns = Namespace("http://lwm.gm.fh-koeln.de/")

  val bindings = Bindings[Sesame](ns)
  import bindings.GroupScheduleAssociationBinding._
  import bindings.uuidBinder

  val groupScheduleAssociation = GroupScheduleAssociation("date", "timetableEntry")
  val groupScheduleAssociationGraph = (
    URI(GroupScheduleAssociation.generateUri(groupScheduleAssociation)).a(lwm.GroupScheduleAssociation)
      -- lwm.date ->- groupScheduleAssociation.date
      -- lwm.timetableEntry ->- groupScheduleAssociation.timetableEntry
      -- lwm.id ->- groupScheduleAssociation.id
    ).graph

  "A GroupScheduleAssociationBindingSpec" should {
    "return a RDF graph representation of a groupScheduleAssociation" in {
      val graph = groupScheduleAssociation.toPG.graph

      graph isIsomorphicWith groupScheduleAssociationGraph shouldBe true
    }
    "return a groupScheduleAssociation based on a RDF graph representation" in {
      val expectedGroupScheduleAssociation = PointedGraph[Rdf](URI(GroupScheduleAssociation.generateUri(groupScheduleAssociation)), groupScheduleAssociationGraph).as[GroupScheduleAssociation]

      expectedGroupScheduleAssociation  match {
        case Success(s) =>
          s shouldEqual groupScheduleAssociation
        case Failure(e) =>
          fail(s"Unable to deserialise groupScheduleAssociation graph: $e")
      }
    }
    }
}
