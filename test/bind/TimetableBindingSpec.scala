package bind

import java.util.UUID

import base.SesameDbSpec
import models._
import org.joda.time.{DateTime, LocalDate, LocalTime}
import org.w3.banana.PointedGraph

import scala.util.{Failure, Success}

class TimetableBindingSpec extends SesameDbSpec {

  import bindings.{TimetableDescriptor, TimetableEntryDescriptor, dateTimeBinder, localDateBinder, localTimeBinder, uuidBinder, uuidRefBinder}
  import ops._

  implicit val timetableBinder = TimetableDescriptor.binder
  implicit val timetableEntryBinder = TimetableEntryDescriptor.binder

  val timetableEntry1 = SesameTimetableEntry(Set(User.randomUUID), SesameRoom.randomUUID, 1, LocalTime.now, LocalTime.now)
  val timetableEntry2 = SesameTimetableEntry(Set(User.randomUUID), SesameRoom.randomUUID, 2, LocalTime.now, LocalTime.now)
  val timetable = SesameTimetable(SesameLabwork.randomUUID, Set(timetableEntry1, timetableEntry2), LocalDate.now, Set.empty[DateTime])

  val timetableGraph = URI(SesameTimetable.generateUri(timetable)).a(lwm.Timetable)
    .--(lwm.labwork).->-(timetable.labwork)(ops, uuidRefBinder(SesameLabwork.splitter))
    .--(lwm.entries).->-(timetable.entries)
    .--(lwm.start).->-(timetable.start)
    .--(lwm.blacklist).->-(timetable.localBlacklist)
    .--(lwm.invalidated).->-(timetable.invalidated)
    .--(lwm.id).->-(timetable.id).graph

  val timetableEntryGraph = URI("#").a(lwm.TimetableEntry)
    .--(lwm.supervisor).->-(timetableEntry1.supervisor)(ops, uuidRefBinder(User.splitter))
    .--(lwm.room).->-(timetableEntry1.room)(ops, uuidRefBinder(SesameRoom.splitter))
    .--(lwm.dayIndex).->-(timetableEntry1.dayIndex)
    .--(lwm.start).->-(timetableEntry1.start)
    .--(lwm.end).->-(timetableEntry1.end).graph

  "A TimetableBindingSpec" should {

    "successfully serialise a timetable" in {
      val t = timetableBinder.fromPG(timetable.toPG)

      t shouldBe Success(timetable)
    }

    "return a timetable based on a RDF graph representation" in {
      val expectedTimetable = PointedGraph[Rdf](URI(SesameTimetable.generateUri(timetable)), timetableGraph).as[SesameTimetable]

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
      val expectedTimetableEntry = PointedGraph[Rdf](URI("#"), timetableEntryGraph).as[SesameTimetableEntry]

      expectedTimetableEntry match {
        case Success(s) =>
          s shouldEqual timetableEntry1
        case Failure(e) =>
          fail(s"Unable to deserialise timetable graph: $e")
      }
    }

    "return a timetable atom based on an RDF representation" in {
      import bindings.{EmployeeDescriptor, LabworkDescriptor, RoomDescriptor, TimetableAtomDescriptor, TimetableDescriptor}

      val labwork = SesameLabwork("labwork", "description", UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID(), subscribable = false, published = false)
      val room1 = SesameRoom("room1", "description1")
      val room2 = SesameRoom("room2", "description2")
      val supervisor1 = SesameEmployee("systemid1", "lastname1", "firstname1", "email1", "status1")
      val supervisor2 = SesameEmployee("systemid2", "lastname2", "firstname2", "email2", "status2")

      val timetableEntry1 = SesameTimetableEntry(Set(supervisor1.id), room1.id, 1, LocalTime.now, LocalTime.now)
      val timetableEntry2 = SesameTimetableEntry(Set(supervisor2.id), room2.id, 4, LocalTime.now, LocalTime.now)
      val timetable = SesameTimetable(labwork.id, Set(timetableEntry1, timetableEntry2), LocalDate.now, Set(DateTime.now, DateTime.now))

      val timetableAtom = SesameTimetableAtom(labwork, Set(
        SesameTimetableEntryAtom(Set(supervisor1), room1, timetableEntry1.dayIndex, timetableEntry1.start, timetableEntry1.end),
        SesameTimetableEntryAtom(Set(supervisor2), room2, timetableEntry2.dayIndex, timetableEntry2.start, timetableEntry2.end)
      ), timetable.start, timetable.localBlacklist, timetable.invalidated, timetable.id)

      repo add labwork
      repo addMany List(room1, room2)
      repo addMany List(supervisor1, supervisor2)
      repo add timetable

      repo.get[SesameTimetableAtom](SesameTimetable.generateUri(timetable)) match {
        case Success(Some(atom)) =>
          atom.labwork shouldEqual timetableAtom.labwork
          atom.start isEqual timetableAtom.start shouldBe true
          atom.invalidated shouldEqual timetableAtom.invalidated
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
