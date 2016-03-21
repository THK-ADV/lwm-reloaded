package bind.schedule

import base.SesameDbSpec
import models.semester.Blacklist
import models.{Room, Labwork, Degree}
import models.schedule.{TimetableEntry, Timetable}
import models.users.User
import org.joda.time.{LocalDate, LocalTime, DateTime}
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import store.Namespace
import store.bind.Bindings
import scala.util.{Success, Failure}

class TimetableBindingSpec extends SesameDbSpec {
  implicit val ns = Namespace("http://lwm.gm.fh-koeln.de/")

  val bindings = Bindings[Sesame](ns)

  import ops._
  import bindings.uuidBinder
  import bindings.jodaLocalDateBinder
  import bindings.jodaLocalTimeBinder
  import bindings.TimetableBinding.timetableBinder
  import bindings.TimetableEntryBinding.timetableEntryBinder
  import bindings.BlacklistBinding.blacklistBinder
  import bindings.uuidRefBinder

  val timetableEntry1 = TimetableEntry(User.randomUUID, Room.randomUUID, Degree.randomUUID, 1, LocalTime.now, LocalTime.now)
  val timetableEntry2 = TimetableEntry(User.randomUUID, Room.randomUUID, Degree.randomUUID, 2, LocalTime.now, LocalTime.now)
  val localBlacklist = Blacklist("local", Set(DateTime.now, DateTime.now), Blacklist.randomUUID)
  val timetable = Timetable(Labwork.randomUUID, Set(timetableEntry1, timetableEntry2), LocalDate.now, localBlacklist, Timetable.randomUUID)

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
