package dao

import models.{PostgresScheduleEntryAtom, ScheduleEntry, ScheduleEntryDb}
import services.{AbstractExpandableDaoSpec, ScheduleEntryDao}
import slick.driver.PostgresDriver.api._
import slick.lifted.TableQuery
import store._

final class ScheduleEntryDaoSpec extends AbstractExpandableDaoSpec[ScheduleEntryTable, ScheduleEntryDb, ScheduleEntry] with ScheduleEntryDao {
  import services.AbstractDaoSpec._

  "A ScheduleEntryDaoSpec also" should {
    "return competitive schedules based on given labwork" in {} // TODO
  }

  override protected def name: String = "scheduleEntry"

  override protected val dbEntity: ScheduleEntryDb = populateScheduleEntry(1)(labworks, rooms, employees, groups).head.copy(supervisor = Set.empty)

  override protected val invalidDuplicateOfDbEntity: ScheduleEntryDb = dbEntity.copy(room = randomRoom.id)

  override protected val invalidUpdateOfDbEntity: ScheduleEntryDb = dbEntity.copy(room = randomRoom.id, labwork = randomLabwork.id, group = randomGroup.id)

  override protected val validUpdateOnDbEntity: ScheduleEntryDb = dbEntity.copy(room = randomRoom.id)

  override protected val dbEntities: List[ScheduleEntryDb] = scheduleEntries

  override protected val lwmEntity: ScheduleEntry = dbEntity.toLwmModel

  override protected val lwmAtom: ScheduleEntry = lwmEntity

  override protected val dependencies: DBIOAction[Unit, NoStream, Effect.Write] = DBIO.seq(
    TableQuery[DegreeTable].forceInsertAll(degrees),
    TableQuery[UserTable].forceInsertAll(employees ++ students),
    TableQuery[SemesterTable].forceInsertAll(semesters),
    TableQuery[CourseTable].forceInsertAll(courses),
    TableQuery[LabworkTable].forceInsertAll(labworks),
    TableQuery[RoomTable].forceInsertAll(rooms),
    TableQuery[GroupTable].forceInsertAll(groups)
  )

  override protected val toAdd: List[ScheduleEntryDb] = List.empty

  override protected val numberOfUpdates: Int = 0

  override protected val numberOfDeletions: Int = 0

  override protected def update(toUpdate: List[ScheduleEntryDb]): List[ScheduleEntryDb] = toUpdate

  override protected def atom(dbModel: ScheduleEntryDb): ScheduleEntry = lwmAtom

  override protected def expanderSpecs(dbModel: ScheduleEntryDb, isDefined: Boolean): DBIOAction[Unit, NoStream, Effect.Read] = DBIO.seq()
}
