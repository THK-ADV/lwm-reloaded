package bind.schedule

import base.SesameDbSpec
import models.semester.Blacklist
import models.{Room, Labwork, Degree}
import models.schedule.{Weekday, TimetableEntry, Timetable}
import models.users.Employee
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

  val timetableEntry1 = TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, 1, LocalTime.now, LocalTime.now, TimetableEntry.randomUUID)
  val timetableEntry2 = TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, 2, LocalTime.now, LocalTime.now, TimetableEntry.randomUUID)
  val localBlacklist = Blacklist(Set(DateTime.now, DateTime.now), Blacklist.randomUUID)
  val timetable = Timetable(Labwork.randomUUID, Set(timetableEntry1, timetableEntry2), LocalDate.now, localBlacklist, Timetable.randomUUID)
  val timetableGraph = URI(Timetable.generateUri(timetable)).a(lwm.Timetable)
    .--(lwm.labwork).->-(timetable.labwork)(ops, uuidRefBinder(Labwork.splitter))
    .--(lwm.entries).->-(timetable.entries)
    .--(lwm.start).->-(timetable.start)
    .--(lwm.blacklist).->-(timetable.localBlacklist)
    .--(lwm.id).->-(timetable.id).graph

  "A TimetableBindingSpec" should {
    "return a RDF graph representation of a timetable" in {
      val graph = timetable.toPG.graph

      graph isIsomorphicWith timetableGraph shouldBe true
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
  }
}
