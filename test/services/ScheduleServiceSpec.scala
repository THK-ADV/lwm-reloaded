package services

import java.util.UUID

import base.TestBaseDefinition
import models.{Room, Labwork, Degree, Group, Semester, Course, AssignmentPlan, AssignmentEntry}
import models.schedule.{TimetableEntry, Timetable}
import models.users.{Student, Employee}
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import org.scalatest.WordSpec
import store.{Namespace, SesameRepository}
import store.bind.Bindings

import scala.language.postfixOps
import scala.util.{Failure, Success}

class ScheduleServiceSpec extends WordSpec with TestBaseDefinition {

  import ScheduleG.dateOrd

  val ns = Namespace("http://lwm.gm.fh-koeln.de/")
  val repo = SesameRepository(ns)
  import repo._
  val bindings = Bindings(ns)

  val scheduleService = new ScheduleService(repo)

  "A ScheduleService " should {

    "should convert from scheduleG to schedule" in {
      import bindings.GroupBinding._

      val entries = Set(
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(1), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(2), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(3), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(4), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(6), TimetableEntry.randomUUID)
      )
      val timetable = Timetable(Labwork.randomUUID, entries, DateTime.now, Set.empty[DateTime], 0, Timetable.randomUUID)

      val groups = Set(
        Group("A", Labwork.randomUUID, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), Group.randomUUID),
        Group("B", Labwork.randomUUID, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), Group.randomUUID),
        Group("C", Labwork.randomUUID, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), Group.randomUUID)
      )

      repo.addMany(groups)

      val scheduleG = scheduleService.populate(1, timetable, groups).head
      val result = scheduleService.toSchedule(scheduleG)

      result.id shouldEqual scheduleG.id
      result.labwork shouldEqual scheduleG.labwork
      result.entries.toVector.sortBy(_.date).zip(scheduleG.entries.toVector.sortBy(_.date)).foreach {
        case (se, seg) => se.group shouldEqual seg.group.id
      }
    }

    "should convert from schedule to scheduleG" in {
      import bindings.GroupBinding._

      val entries = Set(
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(1), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(2), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(3), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(4), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(6), TimetableEntry.randomUUID)
      )
      val timetable = Timetable(Labwork.randomUUID, entries, DateTime.now, Set.empty[DateTime], 0, Timetable.randomUUID)

      val groups = Set(
        Group("A", Labwork.randomUUID, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), Group.randomUUID),
        Group("B", Labwork.randomUUID, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), Group.randomUUID),
        Group("C", Labwork.randomUUID, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), Group.randomUUID)
      )

      repo.addMany(groups)

      val schedule = scheduleService.populate(1, timetable, groups).map(scheduleService.toSchedule).head
      val result = scheduleService.toScheduleG(schedule)

      result match {
        case Some(sg) =>
          sg.id shouldEqual schedule.id
          sg.labwork shouldEqual schedule.labwork
          sg.entries.toVector.sortBy(_.date).zip(schedule.entries.toVector.sortBy(_.date)).foreach {
            case (se, seg) => se.group.id shouldEqual seg.group
          }
        case None => fail("Conversion from scheduleG to schedule failed")
      }
    }

    "return number of appointments based on labwork" in {
      import bindings.LabworkBinding._

      val number = 2
      val assignmentPlan = AssignmentPlan(number, Set.empty[AssignmentEntry])
      val labwork = Labwork("label", "description", Semester.randomUUID, Course.randomUUID, Degree.randomUUID, assignmentPlan)

      repo.add(labwork)

      val appointments = scheduleService.appointments(labwork.id)

      appointments match {
        case Success(s) => s match {
          case Some(app) => app shouldBe number
          case None => fail("There should be some appointments")
        }
        case Failure(e) => fail(s"Unable to retrieve appointments: $e")
      }
    }

    "return empty list of scheduleG's when there are no existing schedules to prepare" in {
      import bindings.CourseBinding._
      import bindings.LabworkBinding._
      import bindings.SemesterBinding._

      val semester = Semester("name", "start", "end", "exam", Semester.randomUUID)
      val course = Course("label", "abbreviation", Employee.randomUUID, 1, Course.randomUUID)
      val assignmentPlan = AssignmentPlan(2, Set.empty[AssignmentEntry])
      val labwork = Labwork("label", "description", semester.id, course.id, Degree.randomUUID, assignmentPlan)

      repo.add(semester)
      repo.add(course)
      repo.add(labwork)

      val existing = scheduleService.competitive(labwork.id)

      existing match {
        case Success(s) => s shouldBe None
        case Failure(e) => fail(s"Unable to retrieve existing schedules: $e")
      }
    }

    "prepare by retrieving and mapping existing schedules to scheduleG's" in {
      import bindings.ScheduleBinding._
      import bindings.CourseBinding._
      import bindings.LabworkBinding._
      import bindings.SemesterBinding._
      import bindings.GroupBinding._

      val course1 = Course("label", "abbreviation", Employee.randomUUID, 1, Course.randomUUID)
      val course2 = Course("label", "abbreviation", Employee.randomUUID, 1, Course.randomUUID)
      val course3 = Course("label", "abbreviation", Employee.randomUUID, 1, Course.randomUUID)

      val semester1 = Semester("name", "start", "end", "exam", Semester.randomUUID)
      val semester2 = Semester("name", "start", "end", "exam", Semester.randomUUID)
      val semester3 = Semester("name", "start", "end", "exam", Semester.randomUUID)

      val assignmentPlan = AssignmentPlan(2, Set.empty[AssignmentEntry])
      val labwork1 = Labwork("label", "description", semester1.id, course1.id, Degree.randomUUID, assignmentPlan)
      val labwork2 = Labwork("label", "description", semester1.id, course2.id, Degree.randomUUID, assignmentPlan)
      val labwork3 = Labwork("label", "description", semester1.id, course3.id, Degree.randomUUID, assignmentPlan)

      val entries = Set(
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(1), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(2), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(3), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(6), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(7), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(8), TimetableEntry.randomUUID)
      )
      val timetable1 = Timetable(labwork1.id, entries, DateTime.now, Set.empty[DateTime], 0, Timetable.randomUUID)
      val timetable2 = Timetable(labwork2.id, entries, DateTime.now, Set.empty[DateTime], 0, Timetable.randomUUID)
      val timetable3 = Timetable(labwork3.id, entries, DateTime.now, Set.empty[DateTime], 0, Timetable.randomUUID)

      val groups = Set(
        Group("A", Labwork.randomUUID, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), Group.randomUUID),
        Group("B", Labwork.randomUUID, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), Group.randomUUID),
        Group("C", Labwork.randomUUID, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), Group.randomUUID)
      )

      val schedule1 = scheduleService.populate(1, timetable1, groups).head
      val schedule2 = scheduleService.populate(1, timetable2, groups).head
      val schedule3 = scheduleService.populate(1, timetable3, groups).head

      repo.addMany(Vector(semester1, semester2, semester3))
      repo.addMany(Vector(course1, course2, course3))
      repo.addMany(Vector(labwork1, labwork2, labwork3))
      repo.addMany(groups.toVector)
      repo.addMany(Vector(schedule1, schedule2).map(scheduleService.toSchedule))

      val result = scheduleService.competitive(labwork3.id)

      result match {
        case Success(s) => s match {
          case Some(ss) =>
            ss should not be empty
            ss.size shouldBe 2

            ss.contains(schedule1) shouldBe true
            ss.contains(schedule2) shouldBe true
            ss.contains(schedule3) shouldBe false

          case None => fail("There should be some existing list of scheduleG's")
        }
        case Failure(e) => fail(s"Unable to retrieve existing schedules: $e")
      }
    }

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

    "evaluate an given schedule when there are no other schedules" in {
      import bindings.LabworkBinding._

      val assignmentPlan = AssignmentPlan(2, Set.empty[AssignmentEntry])
      val labwork = Labwork("label", "description", Semester.randomUUID, Course.randomUUID, Degree.randomUUID, assignmentPlan)

      val entries = Set(
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(1), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(2), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(3), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(6), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(7), TimetableEntry.randomUUID),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(8), TimetableEntry.randomUUID)
      )
      val timetable = Timetable(labwork.id, entries, DateTime.now, Set.empty[DateTime], 0, Timetable.randomUUID)

      val groups = Set(
        Group("A", labwork.id, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), Group.randomUUID),
        Group("B", labwork.id, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), Group.randomUUID),
        Group("C", labwork.id, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), Group.randomUUID)
      )

      repo.add(labwork)

      val schedule = scheduleService.populate(1, timetable, groups).head
      val appointments = scheduleService.appointments(labwork.id).get.get
      val existing = scheduleService.competitive(labwork.id)
      val all = if (existing.isSuccess && existing.get.isEmpty) Vector.empty[ScheduleG] else existing.get.get

      val result = scheduleService.evaluate(schedule, appointments, all)

      result.conflicts shouldBe empty
      result.value shouldBe 0
    }

    "evaluate an given schedule when there are some other schedules" in {
      import bindings.CourseBinding._
      import bindings.SemesterBinding._
      import bindings.LabworkBinding._
      import bindings.ScheduleBinding._
      import bindings.DegreeBinding._
      import bindings.GroupBinding._

      val plan = AssignmentPlan(2, Set.empty[AssignmentEntry])

      val mi = Degree("mi", Degree.randomUUID)

      val ap1 = Course("ap1", "c1", Employee.randomUUID, 1, Course.randomUUID)
      val ma1 = Course("ma1", "c2", Employee.randomUUID, 1, Course.randomUUID)

      val semester1 = Semester("semester1", "start1", "end1", "exam1", Semester.randomUUID)

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
            val t = Timetable(l.id, e, DateTime.now, Set.empty[DateTime], 0, Timetable.randomUUID)
            scheduleService.populate(1, t, g).head
        }
      }

      val s = schedules(Vector(ap1Prak, ma1Prak), Vector(entries1, entries2))
      val ap = s.head
      val ma = s.last

      repo.add(mi)
      repo.addMany(Vector(ap1, ma1))
      repo.add[Semester](semester1)
      repo.addMany(Vector(ap1Prak, ma1Prak))
      repo.addMany(ap.entries.map(_.group))
      repo.add(scheduleService.toSchedule(ap))

      val app = scheduleService.appointments(ma.labwork).get.get
      val all = scheduleService.competitive(ma.labwork).get.get

      val result = scheduleService.evaluate(ma, app, all)
      println(s"conflicts ${result.conflicts.size}")
      println(s"guys ${result.conflicts.map(_.member)}")
      println(s"date ${result.conflicts.map(_.entry.start)}")
      println(s"value ${result.value}")

      result.conflicts should not be empty
      result.value should be < 1000
      result.conflicts.size should be <= result.value
    }
  }

  override protected def beforeEach(): Unit = {
    repo.withConnection { conn =>
      repo.rdfStore.removeGraph(conn, repo.ns)
    }
  }
}
