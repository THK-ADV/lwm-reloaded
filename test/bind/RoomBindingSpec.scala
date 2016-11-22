package bind

import base.SesameDbSpec
import models.Room
import org.w3.banana.PointedGraph

import scala.util.{Failure, Success}

class RoomBindingSpec extends SesameDbSpec {

  import bindings.{
  RoomDescriptor,
  dateTimeBinder,
  uuidBinder}
  import ops._

  implicit val roomBinding = RoomDescriptor.binder

  val room = Room("label", "description")
  val roomGraph = (
    URI(Room.generateUri(room)).a(lwm.Room)
      -- lwm.label ->- room.label
      -- lwm.description ->- room.description
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
