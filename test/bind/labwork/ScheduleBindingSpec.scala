package bind.labwork

import java.util.UUID

import base.SesameDbSpec
import models.Room
import models.labwork.{Group, Labwork, Schedule, ScheduleEntry}
import models.users.User
import org.joda.time.{LocalDate, LocalTime}
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import store.bind.Bindings

import scala.util.{Failure, Success}

class ScheduleBindingSpec extends SesameDbSpec {

  val bindings = Bindings[Sesame](namespace)

  import bindings.ScheduleBinding.scheduleBinder
  import bindings.ScheduleEntryBinding.scheduleEntryBinder
  import bindings.{localDateBinder, localTimeBinder, uuidBinder, uuidRefBinder}
  import ops._
  val labwork = UUID.randomUUID()
  val scheduleEntry = ScheduleEntry(labwork, LocalTime.now, LocalTime.now, LocalDate.now, UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID())
  val schedule = Schedule(labwork, Set(scheduleEntry))

  val scheduleGraph = URI(Schedule.generateUri(schedule)).a(lwm.Schedule)
    .--(lwm.labwork).->-(schedule.labwork)(ops, uuidRefBinder(Labwork.splitter))
    .--(lwm.entries).->-(schedule.entries)
    .--(lwm.published).->-(schedule.published)
    .--(lwm.id).->-(schedule.id).graph

  val scheduleEntryGraph = URI(ScheduleEntry.generateUri(scheduleEntry)).a(lwm.ScheduleEntry)
    .--(lwm.labwork).->-(scheduleEntry.labwork)(ops, uuidRefBinder(Labwork.splitter))
    .--(lwm.start).->-(scheduleEntry.start)
    .--(lwm.end).->-(scheduleEntry.end)
    .--(lwm.date).->-(scheduleEntry.date)
    .--(lwm.room).->-(scheduleEntry.room)(ops, uuidRefBinder(Room.splitter))
    .--(lwm.supervisor).->-(scheduleEntry.supervisor)(ops, uuidRefBinder(User.splitter))
    .--(lwm.group).->-(scheduleEntry.group)(ops, uuidRefBinder(Group.splitter))
    .--(lwm.id).->-(scheduleEntry.id).graph

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
      val expectedScheduleEntry = PointedGraph[Rdf](URI(ScheduleEntry.generateUri(scheduleEntry)), scheduleEntryGraph).as[ScheduleEntry]

      expectedScheduleEntry match {
        case Success(s) =>
          s shouldEqual scheduleEntry
        case Failure(e) =>
          fail(s"Unable to deserialise scheduleEntry graph: $e")
      }
    }
  }
}
