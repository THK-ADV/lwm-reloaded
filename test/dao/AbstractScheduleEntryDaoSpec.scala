package dao

import java.util.UUID

import database._
import models._
import org.joda.time.{LocalDate, LocalTime}
import play.api.inject.guice.GuiceableModule
import slick.jdbc.PostgresProfile.api._
import slick.lifted.TableQuery
import utils.date.DateTimeOps._

final class AbstractScheduleEntryDaoSpec extends AbstractExpandableDaoSpec[ScheduleEntryTable, ScheduleEntryDb, ScheduleEntryLike] {

  import AbstractDaoSpec._

  import scala.concurrent.ExecutionContext.Implicits.global

  override protected def name: String = "scheduleEntry"

  override protected val dbEntity: ScheduleEntryDb = ScheduleEntryDb(
    labworks.head.id,
    LocalTime.now.sqlTime,
    LocalTime.now.plusHours(1).sqlTime,
    LocalDate.now.sqlDate,
    rooms.head.id,
    Set.empty,
    groups.head.id
  )

  override protected val invalidDuplicateOfDbEntity: ScheduleEntryDb =
    dbEntity.copy()

  override protected val invalidUpdateOfDbEntity: ScheduleEntryDb =
    dbEntity.copy(labwork = UUID.randomUUID, group = UUID.randomUUID)

  override protected val validUpdateOnDbEntity: ScheduleEntryDb =
    dbEntity.copy(room = rooms.last.id, start = dbEntity.start.localTime.plusHours(1).sqlTime)

  override protected val dbEntities: List[ScheduleEntryDb] = {
    for {
      labwork <- labworks.slice(1, 5)
      group <- groups.slice(1, 6)
    } yield {
      import scala.util.Random.nextInt

      val date = LocalDate.now.plusDays(nextInt(3))
      val start = LocalTime.now.withHourOfDay(nextInt(20))
      val end = start.plusHours(1)

      ScheduleEntryDb(labwork.id, start.sqlTime, end.plusHours(1).sqlTime, date.sqlDate, randomRoom.id, Set.empty, group.id)
    }
  }

  override protected val lwmAtom: ScheduleEntryLike = atom(dbEntity)

  override protected val dependencies: DBIOAction[Unit, NoStream, Effect.Write] = DBIO.seq(
    TableQuery[DegreeTable].forceInsertAll(degrees),
    TableQuery[UserTable].forceInsertAll(employees ++ students /* ++ privateSupervisors*/),
    TableQuery[SemesterTable].forceInsertAll(semesters),
    TableQuery[CourseTable].forceInsertAll(courses),
    TableQuery[LabworkTable].forceInsertAll(labworks /* ++ privateLabs*/),
    TableQuery[RoomTable].forceInsertAll(rooms),
    TableQuery[GroupTable].forceInsertAll(groups),
    TableQuery[GroupMembershipTable].forceInsertAll(groupMemberships)
  )

  override protected val toAdd: List[ScheduleEntryDb] = {
    for {
      labwork <- labworks.slice(5, 9)
      group <- groups.slice(6, 11)
    } yield {
      import scala.util.Random.nextInt

      val date = LocalDate.now.plusDays(nextInt(3))
      val start = LocalTime.now.withHourOfDay(nextInt(20))
      val end = start.plusHours(1)
      val supervisors = takeSomeOf(employees).map(_.id).toSet

      ScheduleEntryDb(labwork.id, start.sqlTime, end.plusHours(1).sqlTime, date.sqlDate, randomRoom.id, supervisors, group.id)
    }
  }

  override protected val numberOfUpdates: Int = (toAdd.size * 0.3).toInt

  override protected val numberOfDeletions: Int = (toAdd.size * 0.3).toInt

  override protected def update(toUpdate: List[ScheduleEntryDb]): List[ScheduleEntryDb] = toUpdate.map { e =>
    e.copy(room = randomRoom.id, supervisor = takeSomeOf(employees).map(_.id).toSet, date = e.date.localDate.plusDays(1).sqlDate)
  }

  override protected def atom(dbModel: ScheduleEntryDb): ScheduleEntryLike = {
    val labwork = for {
      l <- labworks.find(_.id == dbModel.labwork)
      s <- semesters.find(_.id == l.semester)
      d <- degrees.find(_.id == l.degree)
      c <- courses.find(_.id == l.course)
      lec <- employees.find(_.id == c.lecturer)
      ca = CourseAtom(c.label, c.description, c.abbreviation, lec.toUniqueEntity, c.semesterIndex, c.id)
    } yield LabworkAtom(l.label, l.description, s.toUniqueEntity, ca, d.toUniqueEntity, l.subscribable, l.published, l.id)

    ScheduleEntryAtom(
      labwork.get,
      dbModel.start.localTime,
      dbModel.end.localTime,
      dbModel.date.localDate,
      rooms.find(_.id == dbModel.room).get.toUniqueEntity,
      employees.filter(u => dbModel.supervisor.contains(u.id)).map(_.toUniqueEntity).toSet,
      groups.find(_.id == dbModel.group).get.toUniqueEntity,
      dbModel.id
    )
  }

  override protected def expanderSpecs(dbModel: ScheduleEntryDb, isDefined: Boolean): DBIOAction[Unit, NoStream, Effect.Read] = {
    dao.scheduleEntrySupervisorQuery.filter(_.scheduleEntry === dbModel.id).result.map { sups =>
      sups.map(_.supervisor) should contain theSameElementsAs (if (isDefined) dbModel.supervisor else Nil)
    }
  }

  override protected val dao: ScheduleEntryDao = app.injector.instanceOf(classOf[ScheduleEntryDao])

  override protected def bindings: Seq[GuiceableModule] = Seq.empty
}
