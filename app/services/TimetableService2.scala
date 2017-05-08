package services

import java.util.UUID

import models._
import slick.driver.PostgresDriver
import slick.driver.PostgresDriver.api._
import store.{TimetableBlacklistTable, TimetableEntrySupervisorTable, TimetableEntryTable, TimetableTable}
import models.LwmDateTime._

import scala.concurrent.Future

trait TimetableService2 extends AbstractDao[TimetableTable, TimetableDb, Timetable] {
  import scala.concurrent.ExecutionContext.Implicits.global

  override val tableQuery = TableQuery[TimetableTable]
  protected val timetableBlacklistQuery: TableQuery[TimetableBlacklistTable] = TableQuery[TimetableBlacklistTable]
  protected val timetableEntryQuery: TableQuery[TimetableEntryTable] = TableQuery[TimetableEntryTable]
  protected val timetableEntrySupervisorQuery: TableQuery[TimetableEntrySupervisorTable] = TableQuery[TimetableEntrySupervisorTable]

  override protected def toAtomic(query: Query[TimetableTable, TimetableDb, Seq]): Future[Seq[Timetable]] = ???

  override protected def toUniqueEntity(query: Query[TimetableTable, TimetableDb, Seq]): Future[Seq[Timetable]] = ???

  override protected def setInvalidated(entity: TimetableDb): TimetableDb = ???

  override protected def existsQuery(entity: TimetableDb): Query[TimetableTable, TimetableDb, Seq] = ???

  override protected def shouldUpdate(existing: TimetableDb, toUpdate: TimetableDb): Boolean = ???

  override protected def databaseExpander: Option[DatabaseExpander[TimetableDb]] = Some(new DatabaseExpander[TimetableDb] {
    override def expandCreationOf[X <: Effect](entities: Seq[TimetableDb]) = {
      val timetableEntries = entities.flatMap { timetable =>
        timetable.entries.map { entry =>
          TimetableEntryDb(timetable.id, entry.room, entry.supervisor, entry.dayIndex, entry.start.sqlTime, entry.end.sqlTime, UUID.randomUUID)
        }
      }

      val supervisors = timetableEntries.flatMap { entry =>
        entry.supervisor.map(supervisor => TimetableEntrySupervisor(entry.id, supervisor))
      }

      val blacklists = entities.flatMap(t => t.localBlacklist.map(bl => TimetableBlacklist(t.id, bl)))

      for {
        _ <- timetableBlacklistQuery ++= blacklists
        _ <- timetableEntryQuery ++= timetableEntries
        _ <- timetableEntrySupervisorQuery ++= supervisors
      } yield entities
    }

    override def expandDeleteOf(entity: TimetableDb) = {
      val supervisors = entity.entries.flatMap(_.supervisor)

      /*val x = timetableEntrySupervisorQuery.filter { entrySupervisor =>
        entrySupervisor.timetableEntryFk.map(e => (e.timetable, e.id)).filter {
          case (timetableId, entryId) => timetableId === entity.id && entryId === entrySupervisor.timetableEntry && entrySupervisor.supervisor.inSet(supervisors)
        }
      }*/

      ???
    }

    override def expandUpdateOf(entity: TimetableDb) = ???
  })

  private lazy val schemas = List(
    tableQuery.schema,
    timetableBlacklistQuery.schema,
    timetableEntryQuery.schema,
    timetableEntrySupervisorQuery.schema
  )

  override def createSchema: Future[Unit] = {
    db.run(DBIO.seq(schemas.map(_.create): _*).transactionally)
  }

  override def dropSchema: Future[Unit] = {
    db.run(DBIO.seq(schemas.reverseMap(_.drop): _*).transactionally)
  }
}

final class TimetableService2Impl(val db: PostgresDriver.backend.Database) extends TimetableService2
