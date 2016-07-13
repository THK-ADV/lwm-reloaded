package bind.labwork

import java.util.UUID

import base.SesameDbSpec
import models.labwork._
import models.users.{Employee, User}
import models.{Degree, Room}
import org.joda.time.{DateTime, LocalDate, LocalTime}
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import store.bind.Bindings

import scala.util.{Failure, Success}

class TimetableBindingSpec extends SesameDbSpec {

  import bindings.{
  TimetableDescriptor,
  TimetableEntryDescriptor,
  localDateBinder,
  localTimeBinder,
  uuidBinder,
  uuidRefBinder,
  dateTimeBinder}
  import ops._

  implicit val timetableBinder = TimetableDescriptor.binder
  implicit val timetableEntryBinder = TimetableEntryDescriptor.binder

  val timetableEntry1 = TimetableEntry(User.randomUUID, Room.randomUUID, Degree.randomUUID, 1, LocalTime.now, LocalTime.now)
  val timetableEntry2 = TimetableEntry(User.randomUUID, Room.randomUUID, Degree.randomUUID, 2, LocalTime.now, LocalTime.now)
  val timetable = Timetable(Labwork.randomUUID, Set(timetableEntry1, timetableEntry2), LocalDate.now, Set.empty[DateTime], Timetable.randomUUID)

  val timetableGraph = URI(Timetable.generateUri(timetable)).a(lwm.Timetable)
    .--(lwm.labwork).->-(timetable.labwork)(ops, uuidRefBinder(Labwork.splitter))
    .--(lwm.entries).->-(timetable.entries)
    .--(lwm.start).->-(timetable.start)
    .--(lwm.blacklist).->-(timetable.localBlacklist)
    .--(lwm.id).->-(timetable.id).graph

  val timetableEntryGraph = URI("#").a(lwm.TimetableEntry)
    .--(lwm.supervisor).->-(timetableEntry1.supervisor)(ops, uuidRefBinder(User.splitter))
    .--(lwm.room).->-(timetableEntry1.room)(ops, uuidRefBinder(Room.splitter))
    .--(lwm.degree).->-(timetableEntry1.degree)(ops, uuidRefBinder(Degree.splitter))
    .--(lwm.dayIndex).->-(timetableEntry1.dayIndex)
    .--(lwm.start).->-(timetableEntry1.start)
    .--(lwm.end).->-(timetableEntry1.end).graph

  "A TimetableBindingSpec" should {

    "successfully serialise a timetable" in {
      val t = timetableBinder.fromPG(timetable.toPG)

      t shouldBe Success(timetable)
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

    "successfully serialise a timetable entry" in {
      val te = timetableEntryBinder.fromPG(timetableEntry1.toPG)

      te shouldBe Success(timetableEntry1)
    }

    "return a timetable entry based on a RDF graph representation" in {
      val expectedTimetableEntry = PointedGraph[Rdf](URI("#"), timetableEntryGraph).as[TimetableEntry]

      expectedTimetableEntry match {
        case Success(s) =>
          s shouldEqual timetableEntry1
        case Failure(e) =>
          fail(s"Unable to deserialise timetable graph: $e")
      }
    }

    "return a timetable atom based on an RDF representation" in {
      import bindings.{
      LabworkDescriptor,
      RoomDescriptor,
      DegreeDescriptor,
      EmployeeDescriptor,
      TimetableDescriptor,
      TimetableAtomDescriptor
      }

      val labwork = Labwork("labwork", "description", UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID(), false, false)
      val degree = Degree("degree", "abbr")
      val room1 = Room("room1", "description1")
      val room2 = Room("room2", "description2")
      val supervisor1 = Employee("systemid1", "lastname1", "firstname1", "email1", "status1")
      val supervisor2 = Employee("systemid2", "lastname2", "firstname2", "email2", "status2")

      val timetableEntry1 = TimetableEntry(supervisor1.id, room1.id, degree.id, 1, LocalTime.now, LocalTime.now)
      val timetableEntry2 = TimetableEntry(supervisor2.id, room2.id, degree.id, 4, LocalTime.now, LocalTime.now)
      val timetable = Timetable(labwork.id, Set(timetableEntry1, timetableEntry2), LocalDate.now, Set(DateTime.now, DateTime.now))

      val timetableAtom = TimetableAtom(labwork, Set(
        TimetableEntryAtom(supervisor1, room1, degree, timetableEntry1.dayIndex, timetableEntry1.start, timetableEntry1.end),
        TimetableEntryAtom(supervisor2, room2, degree, timetableEntry2.dayIndex, timetableEntry2.start, timetableEntry2.end)
      ), timetable.start, timetable.localBlacklist, timetable.id)

      repo add labwork
      repo addMany List(room1, room2)
      repo addMany List(supervisor1, supervisor2)
      repo add degree
      repo add timetable

      repo.get[TimetableAtom](Timetable.generateUri(timetable)) match {
        case Success(Some(atom)) =>
          atom.labwork shouldEqual timetableAtom.labwork
          atom.start isEqual timetableAtom.start shouldBe true
          atom.id shouldEqual timetableAtom.id
          atom.entries shouldBe timetableAtom.entries

        case Success(None) =>
          fail("There should exist one timetable")
        case Failure(e) =>
          fail(s"TimetableAtom could not be deserialised: $e")
      }
    }
  }
}
