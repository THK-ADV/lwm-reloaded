package services

import java.util.UUID

import base.TestBaseDefinition
import models.schedule.{Timetable, TimetableEntry}
import models.semester.{Blacklist, Semester}
import models.users.{Employee, Student}
import models.{AssignmentEntry, AssignmentPlan, Course, Degree, Group, Labwork, Room}
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import org.scalatest.WordSpec

import scala.language.postfixOps

class ScheduleServiceSpec extends WordSpec with TestBaseDefinition {

  import ScheduleG.dateOrd

  val scheduleService = new ScheduleService

  "A ScheduleService " should {

    "populate initial schedules once" in {
      val entries = (0 until 6).map(n => TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(n), TimetableEntry.randomUUID)).toSet
      val timetable = Timetable(Labwork.randomUUID, entries, DateTime.now, Blacklist.empty, Timetable.randomUUID)

      val AID = Group.randomUUID
      val BID = Group.randomUUID
      val CID = Group.randomUUID
      val groups = Set(
        Group("A", Labwork.randomUUID, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), AID),
        Group("B", Labwork.randomUUID, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), BID),
        Group("C", Labwork.randomUUID, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), CID)
      )

      val times = 1
      val result = scheduleService.populate(times, timetable, groups)

      result.nonEmpty shouldBe true
      result.size shouldBe times
      result.foreach(f => f.entries.size shouldBe entries.size)
      result.foreach(s => s.entries.count(_.group.id == AID) shouldBe entries.size / groups.size)
      result.foreach(s => s.entries.count(_.group.id == BID) shouldBe entries.size / groups.size)
      result.foreach(s => s.entries.count(_.group.id == CID) shouldBe entries.size / groups.size)

      val gv = groups.toVector.map(_.id)
      result.head.entries.toVector.sortBy(_.date).map(_.group.id).grouped(groups.size).toVector.forall(v1 => v1.forall(gv.contains) && v1.size == gv.size) shouldBe true
    }

    "populate initial schedules any times" in {
      val entries = (0 until 6).map(n => TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(n), TimetableEntry.randomUUID)).toSet
      val timetable = Timetable(Labwork.randomUUID, entries, DateTime.now, Blacklist.empty, Timetable.randomUUID)

      val AID = Group.randomUUID
      val BID = Group.randomUUID
      val CID = Group.randomUUID
      val groups = Set(
        Group("A", Labwork.randomUUID, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), AID),
        Group("B", Labwork.randomUUID, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), BID),
        Group("C", Labwork.randomUUID, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), CID)
      )

      val times = 5
      val result = scheduleService.populate(times, timetable, groups)

      result.nonEmpty shouldBe true
      result.size shouldBe times
      result.foreach(f => f.entries.size shouldBe entries.size)
      result.foreach(s => s.entries.count(_.group.id == AID) shouldBe entries.size / groups.size)
      result.foreach(s => s.entries.count(_.group.id == BID) shouldBe entries.size / groups.size)
      result.foreach(s => s.entries.count(_.group.id == CID) shouldBe entries.size / groups.size)
    }

    "mutate given schedule by swapping two randomly chosen groups" in {
      val entries = (0 until 12).map(n => TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(n), TimetableEntry.randomUUID)).toSet
      val timetable = Timetable(Labwork.randomUUID, entries, DateTime.now, Blacklist.empty, Timetable.randomUUID)

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
      val entries = (0 until 12).map(n => TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(n), TimetableEntry.randomUUID)).toSet
      val timetable = Timetable(Labwork.randomUUID, entries, DateTime.now, Blacklist.empty, Timetable.randomUUID)

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

      val result = scheduleService.mutateDestructive(schedule, Vector(Conflict(ce, s, g)))

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
      val entries = (0 until 12).map(n => TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(n), TimetableEntry.randomUUID)).toSet
      val timetable = Timetable(Labwork.randomUUID, entries, DateTime.now, Blacklist.empty, Timetable.randomUUID)

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
      val entries = (0 until 12).map(n => TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(n), TimetableEntry.randomUUID)).toSet
      val timetable = Timetable(Labwork.randomUUID, entries, DateTime.now, Blacklist.empty, Timetable.randomUUID)

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

    "evaluate an given schedule when there are no other schedules" in {
      val plan = AssignmentPlan(2, Set.empty[AssignmentEntry])
      val labwork = Labwork("label", "description", Semester.randomUUID, Course.randomUUID, Degree.randomUUID, plan)

      val entries = (0 until 6).map(n => TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(n), TimetableEntry.randomUUID)).toSet
      val timetable = Timetable(labwork.id, entries, DateTime.now, Blacklist.empty, Timetable.randomUUID)

      val groups = Set(
        Group("A", labwork.id, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), Group.randomUUID),
        Group("B", labwork.id, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), Group.randomUUID),
        Group("C", labwork.id, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), Group.randomUUID)
      )
      val existing = Vector.empty[ScheduleG]

      val schedule = scheduleService.populate(1, timetable, groups).head
      val result = scheduleService.evaluate(schedule, plan.numberOfEntries, existing)

      result.conflicts shouldBe empty
      result.value shouldBe 0
    }

    "evaluate an given schedule when there are some other schedules" in {
      val plan = AssignmentPlan(2, Set.empty[AssignmentEntry])
      val mi = Degree("mi", Degree.randomUUID)
      val ap1 = Course("ap1", "c1", Employee.randomUUID, 1, Course.randomUUID)
      val ma1 = Course("ma1", "c2", Employee.randomUUID, 1, Course.randomUUID)
      val semester1 = Semester("semester1", "start1", "end1", "exam1", Blacklist.empty, Semester.randomUUID)
      val ap1Prak = Labwork("ap1Prak", "desc1", semester1.id, ap1.id, mi.id, plan)
      val ma1Prak = Labwork("ma1Prak", "desc2", semester1.id, ma1.id, mi.id, plan)

      val ft = DateTimeFormat.forPattern("dd/MM/yyyy HH:mm:ss")
      val fd = DateTimeFormat.forPattern("dd/MM/yyyy")

      val entries1 = Set(
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("27/10/2015"), ft.parseDateTime("27/10/2015 08:00:00"), ft.parseDateTime("27/10/2015 09:00:00"), fd.parseDateTime("27/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("27/10/2015"), ft.parseDateTime("27/10/2015 09:00:00"), ft.parseDateTime("27/10/2015 10:00:00"), fd.parseDateTime("27/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("27/10/2015"), ft.parseDateTime("27/10/2015 10:00:00"), ft.parseDateTime("27/10/2015 11:00:00"), fd.parseDateTime("27/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("29/10/2015"), ft.parseDateTime("29/10/2015 14:00:00"), ft.parseDateTime("29/10/2015 15:00:00"), fd.parseDateTime("29/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("29/10/2015"), ft.parseDateTime("29/10/2015 14:00:00"), ft.parseDateTime("29/10/2015 15:00:00"), fd.parseDateTime("29/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("29/10/2015"), ft.parseDateTime("29/10/2015 15:00:00"), ft.parseDateTime("29/10/2015 16:00:00"), fd.parseDateTime("29/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("29/10/2015"), ft.parseDateTime("29/10/2015 15:00:00"), ft.parseDateTime("29/10/2015 16:00:00"), fd.parseDateTime("29/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("29/10/2015"), ft.parseDateTime("29/10/2015 16:00:00"), ft.parseDateTime("29/10/2015 17:00:00"), fd.parseDateTime("29/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("29/10/2015"), ft.parseDateTime("29/10/2015 16:00:00"), ft.parseDateTime("29/10/2015 17:00:00"), fd.parseDateTime("29/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 08:00:00"), ft.parseDateTime("30/10/2015 09:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 08:00:00"), ft.parseDateTime("30/10/2015 09:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 09:00:00"), ft.parseDateTime("30/10/2015 10:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 09:00:00"), ft.parseDateTime("30/10/2015 10:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 10:00:00"), ft.parseDateTime("30/10/2015 11:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 10:00:00"), ft.parseDateTime("30/10/2015 11:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 11:00:00"), ft.parseDateTime("30/10/2015 12:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 11:00:00"), ft.parseDateTime("30/10/2015 12:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 12:00:00"), ft.parseDateTime("30/10/2015 13:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 12:00:00"), ft.parseDateTime("30/10/2015 13:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 13:00:00"), ft.parseDateTime("30/10/2015 14:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 13:00:00"), ft.parseDateTime("30/10/2015 14:00:00"), fd.parseDateTime("30/10/2015")), // ======
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("03/11/2015"), ft.parseDateTime("03/11/2015 08:00:00"), ft.parseDateTime("03/11/2015 09:00:00"), fd.parseDateTime("03/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("03/11/2015"), ft.parseDateTime("03/11/2015 09:00:00"), ft.parseDateTime("03/11/2015 10:00:00"), fd.parseDateTime("03/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("03/11/2015"), ft.parseDateTime("03/11/2015 10:00:00"), ft.parseDateTime("03/11/2015 11:00:00"), fd.parseDateTime("03/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("05/11/2015"), ft.parseDateTime("05/11/2015 14:00:00"), ft.parseDateTime("05/11/2015 15:00:00"), fd.parseDateTime("05/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("05/11/2015"), ft.parseDateTime("05/11/2015 14:00:00"), ft.parseDateTime("05/11/2015 15:00:00"), fd.parseDateTime("05/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("05/11/2015"), ft.parseDateTime("05/11/2015 15:00:00"), ft.parseDateTime("05/11/2015 16:00:00"), fd.parseDateTime("05/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("05/11/2015"), ft.parseDateTime("05/11/2015 15:00:00"), ft.parseDateTime("05/11/2015 16:00:00"), fd.parseDateTime("05/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("05/11/2015"), ft.parseDateTime("05/11/2015 16:00:00"), ft.parseDateTime("05/11/2015 17:00:00"), fd.parseDateTime("05/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("05/11/2015"), ft.parseDateTime("05/11/2015 16:00:00"), ft.parseDateTime("05/11/2015 17:00:00"), fd.parseDateTime("05/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 08:00:00"), ft.parseDateTime("06/11/2015 09:00:00"), fd.parseDateTime("06/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 08:00:00"), ft.parseDateTime("06/11/2015 09:00:00"), fd.parseDateTime("06/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 09:00:00"), ft.parseDateTime("06/11/2015 10:00:00"), fd.parseDateTime("06/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 09:00:00"), ft.parseDateTime("06/11/2015 10:00:00"), fd.parseDateTime("06/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 10:00:00"), ft.parseDateTime("06/11/2015 11:00:00"), fd.parseDateTime("06/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 10:00:00"), ft.parseDateTime("06/11/2015 11:00:00"), fd.parseDateTime("06/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 11:00:00"), ft.parseDateTime("06/11/2015 12:00:00"), fd.parseDateTime("06/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 11:00:00"), ft.parseDateTime("06/11/2015 12:00:00"), fd.parseDateTime("06/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 12:00:00"), ft.parseDateTime("06/11/2015 13:00:00"), fd.parseDateTime("06/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 12:00:00"), ft.parseDateTime("06/11/2015 13:00:00"), fd.parseDateTime("06/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 13:00:00"), ft.parseDateTime("06/11/2015 14:00:00"), fd.parseDateTime("06/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 13:00:00"), ft.parseDateTime("06/11/2015 14:00:00"), fd.parseDateTime("06/11/2015"))
      )
      val entries2 = Set(
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("26/10/2015"), ft.parseDateTime("26/10/2015 08:00:00"), ft.parseDateTime("26/10/2015 11:00:00"), fd.parseDateTime("26/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("27/10/2015"), ft.parseDateTime("27/10/2015 08:00:00"), ft.parseDateTime("27/10/2015 11:00:00"), fd.parseDateTime("27/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("29/10/2015"), ft.parseDateTime("29/10/2015 08:00:00"), ft.parseDateTime("29/10/2015 11:00:00"), fd.parseDateTime("29/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("29/10/2015"), ft.parseDateTime("29/10/2015 11:00:00"), ft.parseDateTime("29/10/2015 14:00:00"), fd.parseDateTime("29/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 11:00:00"), ft.parseDateTime("30/10/2015 14:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 14:00:00"), ft.parseDateTime("30/10/2015 17:00:00"), fd.parseDateTime("30/10/2015")), // ======
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("02/11/2015"), ft.parseDateTime("02/11/2015 08:00:00"), ft.parseDateTime("02/11/2015 11:00:00"), fd.parseDateTime("02/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("03/11/2015"), ft.parseDateTime("03/11/2015 08:00:00"), ft.parseDateTime("03/11/2015 11:00:00"), fd.parseDateTime("03/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("05/11/2015"), ft.parseDateTime("05/11/2015 08:00:00"), ft.parseDateTime("05/11/2015 11:00:00"), fd.parseDateTime("05/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("05/11/2015"), ft.parseDateTime("05/11/2015 11:00:00"), ft.parseDateTime("05/11/2015 14:00:00"), fd.parseDateTime("05/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 11:00:00"), ft.parseDateTime("06/11/2015 14:00:00"), fd.parseDateTime("06/11/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("06/11/2015"), ft.parseDateTime("06/11/2015 14:00:00"), ft.parseDateTime("06/11/2015 17:00:00"), fd.parseDateTime("06/11/2015"))
      )

      def schedules(labworks: Vector[Labwork], entries: Vector[Set[TimetableEntry]]): Vector[ScheduleG] = {
        import scala.util.Random._

        val numberOfGroups = entries.map(_.size).max / labworks.map(_.assignmentPlan.numberOfEntries).max
        val groupSize = 10
        val students = (0 until numberOfGroups * groupSize).map(_ => Student.randomUUID).toVector

        labworks.zip(entries).map {
          case (l, e) =>
            val count = e.size / l.assignmentPlan.numberOfEntries
            val g = shuffle(students).take(count * groupSize).grouped(groupSize).map(s => Group("", l.id, s.toSet, Group.randomUUID)).toSet
            val t = Timetable(l.id, e, DateTime.now, Blacklist.empty, Timetable.randomUUID)
            scheduleService.populate(1, t, g).head
        }
      }

      val s = schedules(Vector(ap1Prak, ma1Prak), Vector(entries1, entries2))
      val ap = s.head
      val ma = s.last

      val result = scheduleService.evaluate(ma, plan.numberOfEntries, Vector(ap))
      println(s"conflicts ${result.conflicts.size}")
      println(s"guys ${result.conflicts.map(_.member)}")
      println(s"date ${result.conflicts.map(_.entry.start)}")
      println(s"value ${result.value}")

      result.conflicts should not be empty
      result.value should be < 1000
      result.conflicts.size should be <= result.value
    }
  }
}
