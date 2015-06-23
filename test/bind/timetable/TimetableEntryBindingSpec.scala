package bind.timetable

import base.SesameDbSpec
import models.timetable.TimetableEntry
import store.Namespace
import store.bind.Bindings
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame

import scala.util.{Failure, Success}

class TimetableEntryBindingSpec extends SesameDbSpec {
  import ops._
  implicit val ns = Namespace("http://lwm.gm.fh-koeln.de/")

  val bindings = Bindings[Sesame](ns)
  import bindings.TimetableEntryBinding._
  import bindings.uuidBinder

  val timetableEntry = TimetableEntry("supervisor", "room", "startTime", "endTime", TimetableEntry.randomUUID)
  val timetableEntryGraph = (
    URI(TimetableEntry.generateUri(timetableEntry)).a(lwm.TimetableEntry)
      -- lwm.supervisor ->- timetableEntry.supervisor
      -- lwm.room ->- timetableEntry.room
      -- lwm.startTime ->- timetableEntry.startTime
      -- lwm.endTime ->- timetableEntry.endTime
      -- lwm.id ->- timetableEntry.id
    ).graph

  "A TimetableEntryBindingSpec" should {
    "return a RDF graph representation of a timetableEntry" in {
      val graph = timetableEntry.toPG.graph

      graph isIsomorphicWith timetableEntryGraph shouldBe true
    }
    "return a timetableEntry based on a RDF graph representation" in {
      val expectedTimetableEntry = PointedGraph[Rdf](URI(TimetableEntry.generateUri(timetableEntry)), timetableEntryGraph).as[TimetableEntry]

      expectedTimetableEntry match {
        case Success(s) =>
          s shouldEqual timetableEntry
        case Failure(e) =>
          fail(s"Unable to deserialise timetableEntry graph: $e")
      }
    }
    }
}
