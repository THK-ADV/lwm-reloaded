package bind.schedule

import java.util.UUID

import base.SesameDbSpec
import models.labwork.{ScheduleEntry, Schedule, Group, Labwork}
import models.users.User
import models.Room
import org.joda.time.{LocalDate, LocalTime}
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import store.Namespace
import store.bind.Bindings
import scala.util.{Success, Failure}

class ScheduleBindingSpec extends SesameDbSpec {
  implicit val ns = Namespace("http://lwm.gm.fh-koeln.de/")

  val bindings = Bindings[Sesame](ns)

  import ops._
  import bindings.uuidBinder
  import bindings.jodaLocalDateBinder
  import bindings.jodaLocalTimeBinder
  import bindings.ScheduleBinding.scheduleBinder
  import bindings.ScheduleEntryBinding.scheduleEntryBinder
  import bindings.uuidRefBinder

  val scheduleEntry = ScheduleEntry(LocalTime.now, LocalTime.now, LocalDate.now, UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID())
  val schedule = Schedule(Labwork.randomUUID, Set(scheduleEntry))

  val scheduleGraph = URI(Schedule.generateUri(schedule)).a(lwm.Schedule)
    .--(lwm.labwork).->-(schedule.labwork)(ops, uuidRefBinder(Labwork.splitter))
    .--(lwm.entries).->-(schedule.entries)
    .--(lwm.published).->-(schedule.published)
    .--(lwm.id).->-(schedule.id).graph

  val scheduleEntryGraph = URI("#").a(lwm.ScheduleEntry)
    .--(lwm.start).->-(scheduleEntry.start)
    .--(lwm.end).->-(scheduleEntry.end)
    .--(lwm.date).->-(scheduleEntry.date)
    .--(lwm.room).->-(scheduleEntry.room)(ops, uuidRefBinder(Room.splitter))
    .--(lwm.supervisor).->-(scheduleEntry.supervisor)(ops, uuidRefBinder(User.splitter))
    .--(lwm.group).->-(scheduleEntry.group)(ops, uuidRefBinder(Group.splitter)).graph

  "A ScheduleBindingSpec " should {

    "successfully serialise a schedule" in {
      val s = scheduleBinder.fromPG(schedule.toPG)

      s shouldBe Success(schedule)
    }

    "successfully serialise a scheduleEntry" in {
      val se = scheduleEntryBinder.fromPG(scheduleEntry.toPG)

      se shouldBe Success(scheduleEntry)
    }

    "return a schedule based on a RDF graph representation" in {
      val expectedSchedule = PointedGraph[Rdf](URI(Schedule.generateUri(schedule)), scheduleGraph).as[Schedule]

      expectedSchedule match {
        case Success(s) =>
          s shouldEqual schedule
        case Failure(e) =>
          fail(s"Unable to deserialise schedule graph: $e")
      }
    }

    "return a schedule entry based on a RDF graph representation" in {
      val expectedScheduleEntry = PointedGraph[Rdf](URI("#"), scheduleEntryGraph).as[ScheduleEntry]

      expectedScheduleEntry match {
        case Success(s) =>
          s shouldEqual scheduleEntry
        case Failure(e) =>
          fail(s"Unable to deserialise scheduleEntry graph: $e")
      }
    }
  }
}
