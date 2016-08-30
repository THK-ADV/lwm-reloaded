package invalidation.labwork

import java.util.UUID

import base.SesameDbSpec
import models.Room
import models.labwork.{Group, Labwork, Schedule, ScheduleEntry}
import models.users.User
import org.joda.time.{LocalDate, LocalTime}

import scala.util.Random._
import scala.util.Success


class ScheduleInvalidation extends SesameDbSpec {


  "A Schedule invalidation" should {

    def schedEnts(labwork: UUID): Stream[ScheduleEntry] = Stream.continually(
      ScheduleEntry(labwork, LocalTime.now, LocalTime.now plusHours 2, LocalDate.now, Room.randomUUID, Set(User.randomUUID), Group.randomUUID))

    def scheds: Stream[Schedule] = Stream.continually {
      val labwork = Labwork.randomUUID
      Schedule(labwork, (schedEnts(labwork) take 20).toSet)
    }

    "invalidate the schedule and subsequent schedule entries" in {
      import bindings.{ScheduleDescriptor, ScheduleEntryDescriptor}

      val schedules = (scheds take 100).toSet
      val toInvalidate = shuffle(schedules) take 30

      repo.addMany[Schedule](schedules)

      toInvalidate foreach (a => repo.invalidate[Schedule](Schedule.generateUri(a)))

      repo.getAll[Schedule] shouldBe Success(schedules diff toInvalidate)
      repo.getAll[ScheduleEntry] shouldBe Success((schedules flatMap (_.entries)) diff (toInvalidate flatMap (_.entries)))

      repo.deepGetAll[Schedule] map (_ map (_.id)) shouldBe Success(schedules map (_.id))
      repo.deepGetAll[ScheduleEntry] map (_ map (_.id)) shouldBe Success(schedules flatMap (_.entries) map (_.id))
    }
  }
}
