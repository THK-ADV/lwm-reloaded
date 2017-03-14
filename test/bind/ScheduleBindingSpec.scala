package bind

import java.util.UUID

import base.SesameDbSpec
import models._
import models.{SesameSemester$, _}
import org.joda.time.{LocalDate, LocalTime}
import org.w3.banana.PointedGraph

import scala.util.{Failure, Success}

class ScheduleBindingSpec extends SesameDbSpec {

  import bindings.{ScheduleDescriptor, ScheduleEntryDescriptor, dateTimeBinder, localDateBinder, localTimeBinder, uuidBinder, uuidRefBinder}
  import ops._

  implicit val scheduleBinder = ScheduleDescriptor.binder
  implicit val scheduleEntryBinder = ScheduleEntryDescriptor.binder

  val labwork = UUID.randomUUID()
  val scheduleEntry = ScheduleEntry(labwork, LocalTime.now, LocalTime.now, LocalDate.now, UUID.randomUUID(), Set.empty[UUID], UUID.randomUUID())
  val schedule = Schedule(labwork, Set(scheduleEntry))

  val scheduleGraph = URI(Schedule.generateUri(schedule)).a(lwm.Schedule)
    .--(lwm.labwork).->-(schedule.labwork)(ops, uuidRefBinder(SesameLabwork.splitter))
    .--(lwm.entries).->-(schedule.entries)
    .--(lwm.invalidated).->-(schedule.invalidated)
    .--(lwm.id).->-(schedule.id).graph

  val scheduleEntryGraph = URI(ScheduleEntry.generateUri(scheduleEntry)).a(lwm.ScheduleEntry)
    .--(lwm.labwork).->-(scheduleEntry.labwork)(ops, uuidRefBinder(SesameLabwork.splitter))
    .--(lwm.start).->-(scheduleEntry.start)
    .--(lwm.end).->-(scheduleEntry.end)
    .--(lwm.date).->-(scheduleEntry.date)
    .--(lwm.room).->-(scheduleEntry.room)(ops, uuidRefBinder(Room.splitter))
    .--(lwm.supervisor).->-(scheduleEntry.supervisor)(ops, uuidRefBinder(User.splitter))
    .--(lwm.group).->-(scheduleEntry.group)(ops, uuidRefBinder(Group.splitter))
    .--(lwm.invalidated).->-(scheduleEntry.invalidated)
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

    "return a schedule atom based on an RDF representation" in {
      import bindings.{CourseDescriptor, DegreeDescriptor, EmployeeDescriptor, GroupDescriptor, LabworkDescriptor, RoomDescriptor, ScheduleAtomDescriptor, ScheduleDescriptor, ScheduleEntryDescriptor, SemesterDescriptor}

      val semester = SesameSemester("label to pass", "abbrev to pass", LocalDate.now, LocalDate.now, LocalDate.now)
      val employee = SesameEmployee("systemId to pass", "last name to pass", "first name to pass", "email to pass", "employee")
      val course = SesameCourse("label to pass", "desc to pass", "abbrev to pass", employee.id, 1, None, UUID.randomUUID)
      val courseAtom = SesameCourseAtom(course.label, course.description, course.abbreviation, employee, course.semesterIndex, course.invalidated, course.id)
      val degree = PostgresDegree("label to pass", "abbrev to pass")

      val labwork = SesameLabwork("labwork", "description", semester.id, courseAtom.id, degree.id, subscribable = false, published = false)
      val labworkAtom = SesameLabworkAtom(labwork.label, labwork.description, semester, courseAtom, degree, labwork.subscribable, labwork.subscribable, labwork.invalidated, labwork.id)

      val room1 = Room("room1", "description1")
      val room2 = Room("room2", "description2")
      val supervisor1 = SesameEmployee("systemid1", "lastname1", "firstname1", "email1", "status1")
      val supervisor2 = SesameEmployee("systemid2", "lastname2", "firstname2", "email2", "status2")
      val group1 = Group("group1", labwork.id, Set(UUID.randomUUID(), UUID.randomUUID()))
      val group2 = Group("group2", labwork.id, Set(UUID.randomUUID(), UUID.randomUUID()))
      val scheduleEntry1 = ScheduleEntry(labwork.id, LocalTime.now, LocalTime.now, LocalDate.now, room1.id, Set(supervisor1.id), group1.id)
      val scheduleEntry2 = ScheduleEntry(labwork.id, LocalTime.now, LocalTime.now, LocalDate.now, room2.id, Set(supervisor2.id), group2.id)
      val schedule = Schedule(labwork.id, Set(scheduleEntry1, scheduleEntry2))

      val scheduleAtom = ScheduleAtom(labworkAtom, Set(
        ScheduleEntryAtom(labworkAtom, scheduleEntry1.start, scheduleEntry1.end, scheduleEntry1.date, room1, Set(supervisor1), group1, scheduleEntry1.invalidated, scheduleEntry1.id),
        ScheduleEntryAtom(labworkAtom, scheduleEntry2.start, scheduleEntry2.end, scheduleEntry2.date, room2, Set(supervisor2), group2, scheduleEntry2.invalidated, scheduleEntry2.id)
      ), schedule.invalidated, schedule.id)


      repo add labwork
      repo add semester
      repo add employee
      repo add course
      repo add degree
      repo addMany List(room1, room2)
      repo addMany List(supervisor1, supervisor2)
      repo addMany List(group1, group2)
      repo addMany List(scheduleEntry1, scheduleEntry2)
      repo add schedule

      repo.get[ScheduleAtom](Schedule.generateUri(schedule)) match {
        case Success(Some(atom)) =>
          atom shouldEqual scheduleAtom
        case Success(None) =>
          fail("There should exist one schedule")
        case Failure(e) =>
          fail(s"ScheduleAtom could not be deserialised: $e")
      }
    }
  }
}
