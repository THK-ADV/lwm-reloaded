package dao

import java.util.UUID

import dao.AbstractDaoSpec._
import models._
import org.joda.time.{LocalDate, LocalTime}
import store._

final class TimetableDaoSpec extends AbstractExpandableDaoSpec[TimetableTable, TimetableDb, Timetable] with TimetableDao {
  import utils.LwmDateTime._
  import slick.driver.PostgresDriver.api._

  private lazy val privateLabs = populateLabworks(10)(semesters, courses, degrees)
  private lazy val privateBlacklists = populateBlacklists(50)
  private lazy val privateEmployees = populateEmployees(30)

  def timetableEntryAtom(timetable: TimetableDb)(users: List[DbUser], labworks: List[LabworkDb], blacklists: List[BlacklistDb], rooms: List[RoomDb]) = {
    val entryAtoms = timetable.entries.map { e =>
      val supervisors = users.filter(u => e.supervisor.contains(u.id)).map(_.toLwmModel).toSet
      val room = rooms.find(_.id == e.room).get.toLwmModel

      PostgresTimetableEntryAtom(supervisors, room, e.dayIndex, e.start, e.end)
    }

    PostgresTimetableAtom(
      labworks.find(_.id == timetable.labwork).get.toLwmModel,
      entryAtoms,
      timetable.start.localDate,
      blacklists.filter(b => timetable.localBlacklist.contains(b.id)).map(_.toLwmModel).toSet,
      timetable.id
    )
  }

  override protected def name: String = "timetable"

  override protected val dbEntity: TimetableDb = TimetableDb(labworks.head.id, Set.empty, LocalDate.now.sqlDate, Set.empty)

  override protected val invalidDuplicateOfDbEntity: TimetableDb = dbEntity.copy(id = UUID.randomUUID)

  override protected val invalidUpdateOfDbEntity: TimetableDb = dbEntity.copy()

  override protected val validUpdateOnDbEntity: TimetableDb = dbEntity.copy(start = dbEntity.start.localDate.plusDays(1).sqlDate)

  override protected val dbEntities: List[TimetableDb] = timetables

  override protected val lwmEntity: Timetable = dbEntity.toLwmModel

  override protected val lwmAtom: Timetable = timetableEntryAtom(dbEntity)(employees, labworks, blacklists, rooms)

  override protected val dependencies: DBIOAction[Unit, NoStream, Effect.Write] = DBIO.seq(
    TableQuery[UserTable].forceInsertAll(employees ++ privateEmployees),
    TableQuery[RoomTable].forceInsertAll(rooms),
    TableQuery[BlacklistTable].forceInsertAll(blacklists ++ privateBlacklists),
    TableQuery[SemesterTable].forceInsertAll(semesters),
    TableQuery[CourseTable].forceInsertAll(courses),
    TableQuery[DegreeTable].forceInsertAll(degrees),
    TableQuery[LabworkTable].forceInsertAll(privateLabs ++ labworks)
  )

  override protected val toAdd: List[TimetableDb] = populateTimetables(10, 8)(privateEmployees, privateLabs, privateBlacklists)

  override protected val numberOfUpdates: Int = 2

  override protected val numberOfDeletions: Int = 3

  override protected def update(toUpdate: List[TimetableDb]): List[TimetableDb] = {
    val chosen1 = toUpdate.head
    val chosen2 = toUpdate.last

    List(
      chosen1.copy(entries = chosen1.entries.drop(chosen1.entries.size/2), localBlacklist = Set.empty),
      chosen2.copy(entries = chosen2.entries + PostgresTimetableEntry(takeSomeOf(privateEmployees).map(_.id).toSet, randomRoom.id, 10, LocalTime.now.plusHours(10), LocalTime.now.plusHours(11)))
    )
  }

  override protected def atom(dbModel: TimetableDb): Timetable = timetableEntryAtom(dbModel)(privateEmployees, privateLabs, privateBlacklists, rooms)

  override protected def expanderSpecs(dbModel: TimetableDb, isDefined: Boolean): DBIOAction[Unit, NoStream, Effect.Read] = {
    val timetableEntries = timetableEntryQuery.filter(_.timetable === dbModel.id)

    DBIO.seq(
      timetableBlacklistQuery.filter(_.timetable === dbModel.id).flatMap(_.blacklistFk).result.map { blacklists =>
        blacklists.map(_.id).toSet shouldBe (if (isDefined) dbModel.localBlacklist else Set.empty)
      },
      timetableEntries.result.map { entries =>
        entries.map(_.toTimetableEntry).toSet shouldBe (if (isDefined) dbModel.entries.map(_.copy(Set.empty)) else Set.empty)
      },
      timetableEntrySupervisorQuery.filter(_.timetableEntry.in(timetableEntries.map(_.id))).flatMap(_.supervisorFk).result.map { supervisors =>
        supervisors.map(_.id).toSet shouldBe (if (isDefined) dbModel.entries.flatMap(_.supervisor) else Set.empty)
      }
    )
  }
}
