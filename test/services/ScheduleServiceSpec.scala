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

  implicit val dateOrd: Ordering[DateTime] = new Ordering[DateTime] {
    override def compare(x: DateTime, y: DateTime): Int = x.compareTo(y)
  }

  "A ScheduleService " should {

    "populate initial schedules once" in {
      val entries = Set(
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(1), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(2), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(3), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(6), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(7), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(8), TimetableEntry.randomUUID)
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
      result.foreach(s => s.entries.count(_.group.id == AID) shouldBe 2)
      result.foreach(s => s.entries.count(_.group.id == BID) shouldBe 2)
      result.foreach(s => s.entries.count(_.group.id == CID) shouldBe 2)
      val gv = groups.toVector.map(_.id)
      result.head.entries.toVector.sortBy(_.date).map(_.group.id).grouped(groups.size).toVector.forall(v1 => v1.forall(gv.contains) && v1.size == gv.size) shouldBe true
    }

    "populate initial schedules any times" in {
      val entries = Set(
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(1), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(2), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(3), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(6), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(7), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(8), TimetableEntry.randomUUID)
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
      result.foreach(s => s.entries.count(_.group.id == AID) shouldBe 2)
      result.foreach(s => s.entries.count(_.group.id == BID) shouldBe 2)
      result.foreach(s => s.entries.count(_.group.id == CID) shouldBe 2)
    }

    "mutate given schedule by swapping two randomly chosen groups" in {
      val entries = Set(
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(1), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(2), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(3), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(4), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(6), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(7), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(8), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(9), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(13), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(14), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(15), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(16), TimetableEntry.randomUUID)
      )
      val timetable = Timetable(Labwork.randomUUID, entries, DateTime.now, Set.empty[DateTime], 0, Timetable.randomUUID)

      val groups = Set(
        Group("A", Labwork.randomUUID, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), Group.randomUUID),
        Group("B", Labwork.randomUUID, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), Group.randomUUID),
        Group("C", Labwork.randomUUID, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), Group.randomUUID),
        Group("D", Labwork.randomUUID, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), Group.randomUUID)
      )

      val schedule = scheduleService.populate(1, timetable, groups).head
      val result = (0 until 10).map(_ => scheduleService.mutate(schedule)).toVector
      result.foreach(_.entries.toVector.sortBy(_.date).map(e => (e.date, e.group)) should not be schedule.entries.toVector.sortBy(_.date).map(e => (e.date, e.group)))
    }

    "mutate given schedule destructively by rearrange contents of groups for one conflict" in {
      val entries = Set(
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(1), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(2), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(3), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(4), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(6), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(7), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(8), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(9), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(13), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(14), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(15), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(16), TimetableEntry.randomUUID)
      )
      val timetable = Timetable(Labwork.randomUUID, entries, DateTime.now, Set.empty[DateTime], 0, Timetable.randomUUID)
      val sb = Group.randomUUID
      val nb = Group.randomUUID
      val ok1 = Group.randomUUID
      val ok2 = Group.randomUUID
      val groups = Set(
        Group("A", Labwork.randomUUID, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), sb),
        Group("B", Labwork.randomUUID, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), ok1),
        Group("C", Labwork.randomUUID, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), ok2),
        Group("D", Labwork.randomUUID, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), nb)
      )

      val schedule = scheduleService.populate(1, timetable, groups).head
      val ce = schedule.entries.find(_.group.id == sb).get
      val g = groups.find(_.id == sb).get
      val m = g.members.toVector
      val s = Vector(m(1), m(3), m(5))

      val result = scheduleService.mutateDestructive(schedule, Vector(
        Conflict(ce, s, g)
      ))

      result should not be schedule
      result.entries.find(_.group.id == sb).foreach(a => s.foreach(s => a.group.members.find(_ == s) shouldBe None))
      result.entries.find(_.group.id == nb).foreach(a => s.foreach(s => a.group.members.find(_ == s) shouldBe Some(s)))
      result.entries.find(_.group.id == nb).foreach(_.group.members should not be groups.find(_.id == nb).get.members)
      result.entries.find(_.group.id == sb).foreach(_.group.members should not be groups.find(_.id == sb).get.members)
      result.entries.find(_.group.id == ok1).foreach(_.group.members shouldBe groups.find(_.id == ok1).get.members)
      result.entries.find(_.group.id == ok2).foreach(_.group.members shouldBe groups.find(_.id == ok2).get.members)

      val size = entries.size / groups.size
      result.entries.count(_.group.id == sb) shouldBe size
      result.entries.count(_.group.id == nb) shouldBe size
      result.entries.count(_.group.id == ok1) shouldBe size
      result.entries.count(_.group.id == ok2) shouldBe size
    }

    "mutate given schedule destructively by rearrange contents of groups for multiple conflicts" in {
      val entries = Set(
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(1), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(2), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(3), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(4), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(6), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(7), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(8), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(9), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(13), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(14), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(15), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(16), TimetableEntry.randomUUID)
      )
      val timetable = Timetable(Labwork.randomUUID, entries, DateTime.now, Set.empty[DateTime], 0, Timetable.randomUUID)
      val sb1 = Group.randomUUID
      val nb1 = Group.randomUUID
      val sb2 = Group.randomUUID
      val ok = Group.randomUUID
      val groups = Set(
        Group("A", Labwork.randomUUID, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), sb1),
        Group("B", Labwork.randomUUID, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), sb2),
        Group("C", Labwork.randomUUID, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), ok),
        Group("D", Labwork.randomUUID, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), nb1)
      )

      val schedule = scheduleService.populate(1, timetable, groups).head
      val ce1 = schedule.entries.find(_.group.id == sb1).get
      val g1 = groups.find(_.id == sb1).get
      val m1 = g1.members.toVector
      val s1 = Vector(m1(1), m1(3), m1(5))

      val ce2 = schedule.entries.find(_.group.id == sb2).get
      val g2 = groups.find(_.id == sb2).get
      val m2 = g2.members.toVector
      val s2 = Vector(m2(2), m2(4))

      val result = scheduleService.mutateDestructive(schedule, Vector(
        Conflict(ce1, s1, g1),
        Conflict(ce2, s2, g2)
      ))

      result should not be schedule
      result.entries.find(_.group.id == sb1).foreach(a => s1.foreach(s => a.group.members.find(_ == s) shouldBe None))
      result.entries.find(_.group.id == nb1).foreach(a => s1.foreach(s => a.group.members.find(_ == s) shouldBe Some(s)))
      result.entries.find(_.group.id == sb2).foreach(a => s2.foreach(s => a.group.members.find(_ == s) shouldBe None))

      result.entries.find(_.group.id == nb1).foreach(_.group.members should not be groups.find(_.id == nb1).get.members)
      result.entries.find(_.group.id == sb1).foreach(_.group.members should not be groups.find(_.id == sb1).get.members)
      result.entries.find(_.group.id == sb2).foreach(_.group.members should not be groups.find(_.id == sb2).get.members)
      result.entries.find(_.group.id == ok).foreach(_.group.members shouldBe groups.find(_.id == ok).get.members)

      val size = entries.size / groups.size
      result.entries.count(_.group.id == sb1) shouldBe size
      result.entries.count(_.group.id == nb1) shouldBe size
      result.entries.count(_.group.id == sb2) shouldBe size
      result.entries.count(_.group.id == ok) shouldBe size
    }

    "successfully cross two schedules" in {
      val entries = Set(
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(1), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(2), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(3), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(4), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(6), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(7), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(8), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(9), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(13), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(14), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(15), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(16), TimetableEntry.randomUUID)
      )
      val timetable = Timetable(Labwork.randomUUID, entries, DateTime.now, Set.empty[DateTime], 0, Timetable.randomUUID)

      val groups = Set(
        Group("A", Labwork.randomUUID, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), Group.randomUUID),
        Group("B", Labwork.randomUUID, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), Group.randomUUID),
        Group("C", Labwork.randomUUID, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), Group.randomUUID),
        Group("D", Labwork.randomUUID, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), Group.randomUUID)
      )

      val schedules = scheduleService.populate(2, timetable, groups)
      val left = schedules.head
      val right = schedules.last
      val result = scheduleService.crossover(left, right)

      schedules.head should not be result._1
      schedules.last should not be result._2

      result._1.entries.zip(left.entries).count(e => e._1 == e._2) < left.entries.size shouldBe true
      result._2.entries.zip(right.entries).count(e => e._1 == e._2) < right.entries.size shouldBe true

      val leftCount = result._1.entries.count(c => left.entries.contains(c))
      leftCount != left.entries.size && leftCount < left.entries.size && leftCount > 0 && leftCount > left.entries.size / 2 shouldBe true

      val rightCount = result._2.entries.count(c => right.entries.contains(c))
      rightCount != right.entries.size && rightCount < right.entries.size && rightCount > 0 && rightCount > right.entries.size / 2 shouldBe true
    }
  }
}
