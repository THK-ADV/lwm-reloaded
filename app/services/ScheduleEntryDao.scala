package services

import java.util.UUID

import models.{PostgresLabworkAtom, ScheduleEntry, ScheduleEntryDb}
import slick.driver.PostgresDriver
import store.{ScheduleEntrySupervisorTable, ScheduleEntryTable}
import slick.driver.PostgresDriver.api._

import scala.concurrent.Future

trait ScheduleEntryDao extends AbstractDao[ScheduleEntryTable, ScheduleEntryDb, ScheduleEntry] {
  import scala.concurrent.ExecutionContext.Implicits.global

  override val tableQuery = TableQuery[ScheduleEntryTable]
  protected val scheduleEntrySupervisorQuery: TableQuery[ScheduleEntrySupervisorTable] = TableQuery[ScheduleEntrySupervisorTable]

  override protected def toAtomic(query: Query[ScheduleEntryTable, ScheduleEntryDb, Seq]): Future[Seq[ScheduleEntry]] = ???

  override protected def toUniqueEntity(query: Query[ScheduleEntryTable, ScheduleEntryDb, Seq]): Future[Seq[ScheduleEntry]] = ???

  override protected def existsQuery(entity: ScheduleEntryDb): Query[ScheduleEntryTable, ScheduleEntryDb, Seq] = ???

  override protected def shouldUpdate(existing: ScheduleEntryDb, toUpdate: ScheduleEntryDb): Boolean = ???

  /*
  PostgresScheduleEntry(labwork: UUID, start: LocalTime, end: LocalTime, date: LocalDate, room: UUID, supervisor: Set[UUID], group: UUID, id: UUID = UUID.randomUUID)
  PostgresScheduleEntryAtom(labwork: PostgresLabworkAtom, start: LocalTime, end: LocalTime, date: LocalDate, room: PostgresRoom, supervisor: Set[User], group: PostgresGroup, id: UUID)
   */

  private def collectDependencies(query: Query[ScheduleEntryTable, ScheduleEntryDb, Seq]) = {
    for {
      q <- query
      q.join
    } yield 1

    ???
  }

  /*
  val comps = all
        .filter(_.labwork.course.semesterIndex == labwork.course.semesterIndex)
        .filter(_.labwork.semester.id == labwork.semester.id)
        .filter(_.labwork.degree.id == labwork.degree.id)
        .filterNot(_.labwork.id == labwork.id)
   */

  def competitive(labwork: PostgresLabworkAtom) = {
    val comps = for {
      t <- tableQuery
      l <- t.labworkFk if l.id =!= labwork.id
      c <- l.courseFk if c.semesterIndex === labwork.course.semesterIndex
      s <- l.semesterFk if s.id === labwork.semester.id
      d <- l.degreeFk if d.id === labwork.degree.id
    } yield t

    // collectDependencies(comps)
  }
}

final class ScheduleEntryDaoImpl(val db: PostgresDriver.backend.Database) extends ScheduleEntryDao
