package invalidation

import java.util.UUID

import base.SesameDbSpec
import models._
import org.joda.time.{LocalDate, LocalTime}

import scala.util.Random._
import scala.util.{Failure, Success}

class RoomInvalidation extends SesameDbSpec {

  "A Room invalidation" should {

    def rce(room: UUID): Stream[SesameReportCardEntry] = Stream.continually {
      if (nextBoolean()) SesameReportCardEntry(User.randomUUID, SesameLabwork.randomUUID, "Label", LocalDate.now, LocalTime.now, LocalTime.now plusHours 2, room, Set())
      else SesameReportCardEntry(User.randomUUID, SesameLabwork.randomUUID, "Label", LocalDate.now, LocalTime.now, LocalTime.now plusHours 2, SesameRoom.randomUUID, Set())
    }

    def sce(room: UUID): Stream[ScheduleEntry] = Stream.continually {
      if (nextBoolean()) ScheduleEntry(SesameLabwork.randomUUID, LocalTime.now, LocalTime.now plusHours 2, LocalDate.now, room, Set(User.randomUUID), Group.randomUUID)
      else ScheduleEntry(SesameLabwork.randomUUID, LocalTime.now, LocalTime.now plusHours 2, LocalDate.now, SesameRoom.randomUUID, Set(User.randomUUID), Group.randomUUID)
    }

    def tte(room: UUID): Stream[SesameTimetableEntry] = Stream.continually(SesameTimetableEntry(Set(User.randomUUID), room, 1, LocalTime.now, LocalTime.now plusHours 2))

    def tt(room: UUID): Stream[SesameTimetable] = Stream.continually {
      if (nextBoolean()) SesameTimetable(SesameLabwork.randomUUID, (tte(room) take 20).toSet, LocalDate.now, Set())
      else SesameTimetable(SesameLabwork.randomUUID, (tte(SesameRoom.randomUUID) take 20).toSet, LocalDate.now, Set())
    }



    "invalidate a room and referencing schedule, report card and timetable entries" in {
      import bindings.{ReportCardEntryDescriptor, RoomDescriptor, ScheduleEntryDescriptor, TimetableDescriptor}

      val room = SesameRoom("Room", "Description")
      val reportCardEntries = (rce(room.id) take 100).toSet
      val scheduleEntries = (sce(room.id) take 100).toSet
      val timetables = (tt(room.id) take 100).toSet

      repo.add[SesameRoom](room)
      repo.addMany[SesameReportCardEntry](reportCardEntries)
      repo.addMany[ScheduleEntry](scheduleEntries)
      repo.addMany[SesameTimetable](timetables)

      repo.invalidate[SesameRoom](SesameRoom.generateUri(room))

      repo.get[SesameRoom](SesameRoom.generateUri(room)) shouldBe Success(None)
      repo.getAll[SesameReportCardEntry] shouldBe Success(reportCardEntries filter (_.room != room.id))
      repo.getAll[ScheduleEntry] shouldBe Success(scheduleEntries filter (_.room != room.id))
      repo.getAll[SesameTimetable] match {
        case Success(set) =>
          set shouldBe
            (timetables map { tt =>
              SesameTimetable(tt.labwork, tt.entries filter (_.room != room.id), tt.start, tt.localBlacklist, tt.invalidated, tt.id)
            })
        case Failure(e) => fail("no")
      }

      repo.deepGet[SesameRoom](SesameRoom.generateUri(room)) map (_ map (_.id)) shouldBe Success(Some(room.id))
      repo.deepGetAll[SesameReportCardEntry] map (_ map (_.id)) shouldBe Success(reportCardEntries map (_.id))
      repo.deepGetAll[SesameTimetable] map (_ map (_.id)) shouldBe Success(timetables map (_.id))
      repo.deepGetAll[ScheduleEntry] map (_ map (_.id)) shouldBe Success(scheduleEntries map (_.id))
    }
  }
}
