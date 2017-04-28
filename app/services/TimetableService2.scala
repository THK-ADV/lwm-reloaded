package services

import models.{Timetable, TimetableDb}
import slick.driver.PostgresDriver
import slick.driver.PostgresDriver.api._
import store.{TimetableBlacklistTable, TimetableEntrySupervisorTable, TimetableEntryTable, TimetableTable}

import scala.concurrent.Future

trait TimetableService2 extends AbstractDao[TimetableTable, TimetableDb, Timetable] {

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
    override def expandCreationOf(entities: Seq[TimetableDb]) = ???

    override def expandDeleteOf(entity: TimetableDb) = ???

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
