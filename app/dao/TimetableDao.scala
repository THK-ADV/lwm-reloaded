package dao

import java.util.UUID

import dao.helper.{DatabaseExpander, TableFilterable}
import database._
import javax.inject.Inject
import models._
import slick.jdbc
import slick.jdbc.PostgresProfile
import slick.jdbc.PostgresProfile.api._
import utils.date.DateTimeOps._

import scala.concurrent.{ExecutionContext, Future}

trait TimetableDao extends AbstractDao[TimetableTable, TimetableDb, TimetableLike] with TableFilterable[TimetableTable] {
  import TableFilterable.labworkFilter

  override val tableQuery = TableQuery[TimetableTable]

  val timetableBlacklistQuery: TableQuery[TimetableBlacklistTable] = TableQuery[TimetableBlacklistTable]
  val timetableEntryQuery: TableQuery[TimetableEntryTable] = TableQuery[TimetableEntryTable]
  val timetableEntrySupervisorQuery: TableQuery[TimetableEntrySupervisorTable] = TableQuery[TimetableEntrySupervisorTable]

  override protected def toAtomic(query: Query[TimetableTable, TimetableDb, Seq]): Future[Seq[TimetableLike]] = collectDependencies(query) {
    case (timetable, labwork, blacklists, entries) =>
      val timetableEntries = entries.map {
        case ((e, r), s) => TimetableEntryAtom(s.map(_.toUniqueEntity).toSet, r.toUniqueEntity, e.dayIndex, e.start.localTime, e.end.localTime)
      }

      TimetableAtom(labwork.toUniqueEntity, timetableEntries.toSet, timetable.start.localDate, blacklists.map(_.toUniqueEntity).toSet, timetable.id)
  }

  override protected def toUniqueEntity(query: Query[TimetableTable, TimetableDb, Seq]): Future[Seq[TimetableLike]] = collectDependencies(query) {
    case (timetable, labwork, blacklists, entries) => buildLwmEntity(timetable, labwork, blacklists, entries)
  }

  def withBlacklists(tableFilter: List[TableFilterPredicate]) = collectDependencies(filterBy(tableFilter)) {
    case (timetable, labwork, blacklists, entries) => (buildLwmEntity(timetable, labwork, blacklists, entries), blacklists.map(_.toUniqueEntity))
  }

  private def buildLwmEntity(timetable: TimetableDb, labwork: LabworkDb, blacklists: Seq[BlacklistDb], entries: Map[(TimetableEntryDb, RoomDb), Seq[UserDb]]) = {
    val timetableEntries = entries.map {
      case ((e, _), s) => TimetableEntry(s.map(_.id).toSet, e.room, e.dayIndex, e.start.localTime, e.end.localTime)
    }

    Timetable(labwork.id, timetableEntries.toSet, timetable.start.localDate, blacklists.map(_.id).toSet, timetable.id)
  }

  private final def collectDependencies[A](query: Query[TimetableTable, TimetableDb, Seq])
    (build: (TimetableDb, LabworkDb, Seq[BlacklistDb], Map[(TimetableEntryDb, RoomDb), Seq[UserDb]]) => A) = {
    val mandatory = for {
      q <- query
      l <- q.labworkFk
    } yield (q, l)

    val innerBlacklist = timetableBlacklistQuery.join(TableQuery[BlacklistTable]).on(_.blacklist === _.id)
    val innerSupervisor = timetableEntrySupervisorQuery.join(TableQuery[UserTable]).on(_.supervisor === _.id)
    val innerTimetableEntry = timetableEntryQuery.join(TableQuery[RoomTable]).on(_.room === _.id).joinLeft(innerSupervisor).on(_._1.id === _._1.timetableEntry)

    val action = mandatory.joinLeft(innerBlacklist).on(_._1.id === _._1.timetable).joinLeft(innerTimetableEntry).on(_._1._1.id === _._1._1.timetable).map {
      case ((t, bl), x) => (t, bl.map(_._2), x.map(y => (y._1, y._2.map(_._2))))
    }.result.map(_.groupBy(_._1).map {
      case ((timetable, labwork), dependencies) =>
        val blacklists = dependencies.flatMap(_._2)
        val entries = dependencies.flatMap(_._3).groupBy(_._1).mapValues(_.flatMap(_._2))

        build(timetable, labwork, blacklists, entries)
    }.toSeq)

    db.run(action)
  }

  override protected def existsQuery(entity: TimetableDb): Query[TimetableTable, TimetableDb, Seq] = {
    filterBy(List(labworkFilter(entity.labwork)))
  }

  override protected def shouldUpdate(existing: TimetableDb, toUpdate: TimetableDb): Boolean = {
    import utils.date.DateTimeOps.SqlDateConverter

    (existing.start.localDate != toUpdate.start.localDate ||
      existing.entries != toUpdate.entries ||
      existing.localBlacklist != toUpdate.localBlacklist) &&
      existing.labwork == toUpdate.labwork
  }

  override protected val databaseExpander: Option[DatabaseExpander[TimetableDb]] = Some(new DatabaseExpander[TimetableDb] {
    override def expandCreationOf[X <: Effect](entities: TimetableDb*): jdbc.PostgresProfile.api.DBIOAction[Seq[TimetableDb], jdbc.PostgresProfile.api.NoStream, Effect.Write with Any] = {
      val timetableEntries = entities.flatMap { timetable =>
        timetable.entries.map { entry =>
          TimetableEntryDb(timetable.id, entry.room, entry.supervisor, entry.dayIndex, entry.start.sqlTime, entry.end.sqlTime)
        }
      }

      for {
        _ <- timetableEntryQuery ++= timetableEntries
        _ <- timetableEntrySupervisorQuery ++= timetableEntries.flatMap { entry =>
          entry.supervisor.map(supervisor => TimetableEntrySupervisor(entry.id, supervisor))
        }
        _ <- timetableBlacklistQuery ++= entities.flatMap(t => t.localBlacklist.map(bl => TimetableBlacklist(t.id, bl)))
      } yield entities
    }

    override def expandDeleteOf(entity: TimetableDb) = {
      val timetableEntries = timetableEntryQuery.filter(_.timetable === entity.id).map(_.id)

      for {
        _ <- timetableEntrySupervisorQuery.filter(_.timetableEntry in timetableEntries).delete
        _ <- timetableEntryQuery.filter(_.id in timetableEntries).delete
        _ <- timetableBlacklistQuery.filter(_.timetable === entity.id).delete
      } yield entity
    }

    override def expandUpdateOf(entity: TimetableDb) = for {
      d <- expandDeleteOf(entity)
      c <- expandCreationOf(d)
    } yield c.head
  })

  override protected val schemas: List[PostgresProfile.DDL] = List(
    tableQuery.schema,
    timetableBlacklistQuery.schema,
    timetableEntryQuery.schema,
    timetableEntrySupervisorQuery.schema
  )
}

final class TimetableDaoImpl @Inject()(val db: Database, val executionContext: ExecutionContext) extends TimetableDao
