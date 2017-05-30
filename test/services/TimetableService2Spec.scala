package services

import java.util.UUID

import models._
import org.joda.time.{LocalDate, LocalTime}
import services.AbstractDaoSpec._
import store._

final class TimetableService2Spec extends AbstractDaoSpec[TimetableTable, TimetableDb, Timetable] with TimetableService2 {
  import slick.driver.PostgresDriver.api._
  import models.LwmDateTime._

  private val privateLabs = populateLabworks(10)
  private val privateBlacklists = populateBlacklists(50)
  private val privateEmployees = populateEmployees(30)

  def atom(timetable: TimetableDb)(users: List[DbUser], labworks: List[LabworkDb], blacklists: List[BlacklistDb], rooms: List[RoomDb]) = {
    val entryAtoms = timetable.entries.map { e =>
      val supervisors = users.filter(u => e.supervisor.contains(u.id)).map(_.toUser).toSet
      val room = rooms.find(_.id == e.room).get.toRoom

      PostgresTimetableEntryAtom(supervisors, room, e.dayIndex, e.start, e.end)
    }

    PostgresTimetableAtom(
      labworks.find(_.id == timetable.labwork).get.toLabwork,
      entryAtoms,
      timetable.start.localDate,
      blacklists.filter(b => timetable.localBlacklist.contains(b.id)).map(_.toBlacklist).toSet,
      timetable.id
    )
  }

  "A TimetableService2Spec also" should {

    val timetables = populateTimetables(10, 8)(privateEmployees, privateLabs, privateBlacklists)

    def assertEverythingOf(timetables: List[TimetableDb], isDefined: Boolean) {
      val ids = timetables.map(_.id)
      val ts = timetables.map(_.toTimetable)
      val tas = timetables.map(t => atom(t)(privateEmployees, privateLabs, privateBlacklists, rooms))

      await(getMany(ids, atomic = false)).count(ts.contains) shouldBe (if (isDefined) ts.size else 0)
      await(getMany(ids)).count(tas.contains) shouldBe (if (isDefined) tas.size else 0)

      timetables.foreach { t =>
        val timetableEntries = timetableEntryQuery.filter(_.timetable === t.id)

        run(DBIO.seq(
          timetableBlacklistQuery.filter(_.timetable === t.id).flatMap(_.blacklistFk).result.map { blacklists =>
            blacklists.map(_.id).toSet shouldBe (if (isDefined) t.localBlacklist else Set.empty)
          }
        ).andThen(
          timetableEntries.result.map { entries =>
            entries.map(_.toTimetableEntry).toSet shouldBe (if (isDefined) t.entries.map(_.copy(Set.empty)) else Set.empty)
          }
        ).andFinally(
          timetableEntrySupervisorQuery.filter(_.timetableEntry.in(timetableEntries.map(_.id))).flatMap(_.supervisorFk).result.map { supervisors =>
            supervisors.map(_.id).toSet shouldBe (if (isDefined) t.entries.flatMap(_.supervisor) else Set.empty)
          }
        ))
      }
    }

    "create timetables with blacklists, entries and supervisors" in {
      await(createMany(timetables)) shouldBe timetables

      assertEverythingOf(timetables, isDefined = true)
    }

    "update a timetable with blacklists, entries and supervisors" in {
      val chosen1 = timetables.head
      val chosen2 = timetables.last

      val updated = List(
        chosen1.copy(entries = chosen1.entries.drop(chosen1.entries.size/2), localBlacklist = Set.empty),
        chosen2.copy(entries = chosen2.entries + PostgresTimetableEntry(takeSomeOf(privateEmployees).map(_.id).toSet, randomRoom.id, 10, LocalTime.now.plusHours(10), LocalTime.now.plusHours(11)))
      )

      await(updateMany(updated)) shouldBe updated.map(Some(_))
      assertEverythingOf(updated, isDefined = true)
    }

    "delete a timetable with blacklists, entries and supervisors" in {
      val chosen = takeSomeOf(timetables).toList

      await(deleteManyEntities(chosen)).flatMap(_.flatMap(_.invalidated)).size shouldBe chosen.size
      assertEverythingOf(chosen, isDefined = false)
    }
  }

  override protected def name: String = "timetable"

  override protected val dbEntity: TimetableDb = TimetableDb(labworks.head.id, Set.empty, LocalDate.now.sqlDate, Set.empty)

  override protected val invalidDuplicateOfDbEntity: TimetableDb = dbEntity.copy(id = UUID.randomUUID)

  override protected val invalidUpdateOfDbEntity: TimetableDb = dbEntity.copy()

  override protected val validUpdateOnDbEntity: TimetableDb = dbEntity.copy(start = dbEntity.start.localDate.plusDays(1).sqlDate)

  override protected val dbEntities: List[TimetableDb] = timetables

  override protected val lwmEntity: Timetable = dbEntity.toTimetable

  override protected val lwmAtom: Timetable = atom(dbEntity)(employees, labworks, blacklists, rooms)

  override protected val dependencies: DBIOAction[Unit, NoStream, Effect.Write] = DBIO.seq(
    TableQuery[UserTable].forceInsertAll(employees ++ privateEmployees),
    TableQuery[RoomTable].forceInsertAll(rooms),
    TableQuery[BlacklistTable].forceInsertAll(blacklists ++ privateBlacklists),
    TableQuery[SemesterTable].forceInsertAll(semesters),
    TableQuery[CourseTable].forceInsertAll(courses),
    TableQuery[DegreeTable].forceInsertAll(degrees),
    TableQuery[LabworkTable].forceInsertAll(privateLabs ++ labworks)
  )
}
