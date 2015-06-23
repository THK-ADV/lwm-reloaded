package bind

import base.SesameDbSpec
import models.Group
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import store.Namespace
import store.bind.Bindings

import scala.util.{Failure, Success}

class GroupBindingSpec extends SesameDbSpec {
  import ops._
  implicit val ns = Namespace("http://lwm.gm.fh-koeln.de/")

  val bindings = Bindings[Sesame](ns)
  import bindings.GroupBinding._
  import bindings.uuidBinder

  val group = Group("GroupSchedule", "Label", "Labwork", Group.randomUUID)
  val groupGraph = (
    URI(Group.generateUri(group)).a(lwm.Group)
      -- lwm.groupSchedule ->- group.groupSchedule
      -- lwm.label ->- group.label
      -- lwm.labwork ->- group.labwork
      -- lwm.id ->- group.id
    ).graph

  "A GroupBindingSpec" should {
    "return a RDF graph representation of a group" in {
      val graph = group.toPG.graph

      graph isIsomorphicWith groupGraph shouldBe true
    }
    "return a group based on a RDF graph representation" in {
      val expectedGroup = PointedGraph[Rdf](URI(Group.generateUri(group)), groupGraph).as[Group]

      expectedGroup match {
        case Success(s) =>
          s shouldEqual group
        case Failure(e) =>
          fail(s"Unable to deserialise group graph: $e")
      }
    }
    }

}
