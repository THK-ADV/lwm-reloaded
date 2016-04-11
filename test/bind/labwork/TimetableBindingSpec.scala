package bind.labwork

import base.SesameDbSpec
import models.labwork.{Labwork, Timetable, TimetableEntry}
import models.users.User
import models.{Degree, Room}
import org.joda.time.{DateTime, LocalDate, LocalTime}
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import store.bind.Bindings

import scala.util.{Failure, Success}

class TimetableBindingSpec extends SesameDbSpec {

  val bindings = Bindings[Sesame](namespace)

  import bindings.TimetableBinding.timetableBinder
  import bindings.TimetableEntryBinding.timetableEntryBinder
  import bindings.{localDateBinder, localTimeBinder, uuidBinder, uuidRefBinder, dateTimeBinder}
  import ops._

  val timetableEntry1 = TimetableEntry(User.randomUUID, Room.randomUUID, Degree.randomUUID, 1, LocalTime.now, LocalTime.now)
  val timetableEntry2 = TimetableEntry(User.randomUUID, Room.randomUUID, Degree.randomUUID, 2, LocalTime.now, LocalTime.now)
  val timetable = Timetable(Labwork.randomUUID, Set(timetableEntry1, timetableEntry2), LocalDate.now, Set.empty[DateTime], Timetable.randomUUID)

  val timetableGraph = URI(Timetable.generateUri(timetable)).a(lwm.Timetable)
    .--(lwm.labwork).->-(timetable.labwork)(ops, uuidRefBinder(Labwork.splitter))
    .--(lwm.entries).->-(timetable.entries)
    .--(lwm.start).->-(timetable.start)
    .--(lwm.blacklist).->-(timetable.localBlacklist)
    .--(lwm.id).->-(timetable.id).graph

  val timetableEntryGraph = URI("#").a(lwm.TimetableEntry)
    .--(lwm.supervisor).->-(timetableEntry1.supervisor)(ops, uuidRefBinder(User.splitter))
    .--(lwm.room).->-(timetableEntry1.room)(ops, uuidRefBinder(Room.splitter))
    .--(lwm.degree).->-(timetableEntry1.degree)(ops, uuidRefBinder(Degree.splitter))
    .--(lwm.dayIndex).->-(timetableEntry1.dayIndex)
    .--(lwm.start).->-(timetableEntry1.start)
    .--(lwm.end).->-(timetableEntry1.end).graph

  "A TimetableBindingSpec" should {

    "successfully serialise a timetable" in {
      val t = timetableBinder.fromPG(timetable.toPG)

      t shouldBe Success(timetable)
    }

    "return a timetable based on a RDF graph representation" in {
      val expectedTimetable = PointedGraph[Rdf](URI(Timetable.generateUri(timetable)), timetableGraph).as[Timetable]

      expectedTimetable match {
        case Success(s) =>
          s shouldEqual timetable
        case Failure(e) =>
          fail(s"Unable to deserialise timetable graph: $e")
      }
    }

    "successfully serialise a timetable entry" in {
      val te = timetableEntryBinder.fromPG(timetableEntry1.toPG)

      te shouldBe Success(timetableEntry1)
    }

    "return a timetable entry based on a RDF graph representation" in {
      val expectedTimetableEntry = PointedGraph[Rdf](URI("#"), timetableEntryGraph).as[TimetableEntry]

      expectedTimetableEntry match {
        case Success(s) =>
          s shouldEqual timetableEntry1
        case Failure(e) =>
          fail(s"Unable to deserialise timetable graph: $e")
      }
    }
  }
}
