package services

import java.util.UUID

import base.TestBaseDefinition
import models.{Degree, Room, Labwork, Group}
import models.schedule.{TimetableEntry, Timetable}
import models.users.Employee
import org.joda.time.DateTime
import org.scalatest.WordSpec

class ScheduleServiceSpec extends WordSpec with TestBaseDefinition {

  val scheduleService = new ScheduleService()

  "A ScheduleService " should {

    "populate initial schedules once" in {
      val entries = Set(
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now, TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now, TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now, TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now, TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now, TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now, TimetableEntry.randomUUID)
      )
      val timetable = Timetable(Labwork.randomUUID, entries, DateTime.now, Set.empty[DateTime], 0, Timetable.randomUUID)

      val AID = Group.randomUUID
      val BID = Group.randomUUID
      val CID = Group.randomUUID
      val groups = Set(
        Group("A", Labwork.randomUUID, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), AID),
        Group("B", Labwork.randomUUID, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), BID),
        Group("C", Labwork.randomUUID, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), CID)
      )

      val result = scheduleService.populate(1, timetable, groups)

      result.nonEmpty shouldBe true
      result.foreach(f => f.entries.size shouldBe entries.size)
      result.foreach(s => s.entries.count(_.group == AID) shouldBe 2)
      result.foreach(s => s.entries.count(_.group == BID) shouldBe 2)
      result.foreach(s => s.entries.count(_.group == CID) shouldBe 2)
    }

    "populate initial schedules any times" in {
      val entries = Set(
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now, TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now, TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now, TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now, TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now, TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now, TimetableEntry.randomUUID)
      )
      val timetable = Timetable(Labwork.randomUUID, entries, DateTime.now, Set.empty[DateTime], 0, Timetable.randomUUID)

      val AID = Group.randomUUID
      val BID = Group.randomUUID
      val CID = Group.randomUUID
      val groups = Set(
        Group("A", Labwork.randomUUID, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), AID),
        Group("B", Labwork.randomUUID, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), BID),
        Group("C", Labwork.randomUUID, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), CID)
      )

      val result = scheduleService.populate(5, timetable, groups)

      result.nonEmpty shouldBe true
      result.size shouldBe 5
      result.foreach(f => f.entries.size shouldBe entries.size)
      result.foreach(s => s.entries.count(_.group == AID) shouldBe 2)
      result.foreach(s => s.entries.count(_.group == BID) shouldBe 2)
      result.foreach(s => s.entries.count(_.group == CID) shouldBe 2)
    }

    "mutate given schedule by swapping groups" in {
      val entries = Set(
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now, TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now, TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now, TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now, TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now, TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now, TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now, TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now, TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now, TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now, TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now, TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now, TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now, TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now, TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now, TimetableEntry.randomUUID)
      )
      val timetable = Timetable(Labwork.randomUUID, entries, DateTime.now, Set.empty[DateTime], 0, Timetable.randomUUID)

      val groups = Set(
        Group("A", Labwork.randomUUID, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), Group.randomUUID),
        Group("B", Labwork.randomUUID, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), Group.randomUUID),
        Group("C", Labwork.randomUUID, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), Group.randomUUID)
      )

      val schedule = scheduleService.populate(1, timetable, groups).head

      val result = (0 until 10).map(_ => scheduleService.mutate(schedule)).toVector

      result.foreach(s => s.entries.toVector.map(_.group) should not be schedule.entries.toVector.map(_.group))
    }
  }
}
