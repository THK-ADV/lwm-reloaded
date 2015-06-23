package bind

import base.SesameDbSpec
import models.Room
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import store.Namespace
import store.bind.Bindings

import scala.util.{Failure, Success}

class RoomBindingSpec extends SesameDbSpec {
  import ops._
  implicit val ns = Namespace("http://lwm.gm.fh-koeln.de/")

  val bindings = Bindings[Sesame](ns)
  import bindings.uuidBinder
  import bindings.RoomBinding._

  val room = Room("label", Room.randomUUID)
  val roomGraph = (
    URI(Room.generateUri(room)).a(lwm.Room)
      -- lwm.label ->- room.label
      -- lwm.id ->- room.id
    ).graph

  "A RoomBindingSpec" should {
    "return a RDF graph representation of a room" in {
      val graph = room.toPG.graph

      graph isIsomorphicWith roomGraph shouldBe true
    }
    "return a room based on a RDF graph representation" in {
      val expectedRoom = PointedGraph[Rdf](URI(Room.generateUri(room)), roomGraph).as[Room]

      expectedRoom match {
        case Success(s) =>
          s shouldEqual room
        case Failure(e) =>
          fail(s"Unable to deserialise room graph: $e")
      }
    }

    }
}
