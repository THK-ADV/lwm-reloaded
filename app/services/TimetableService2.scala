package services

import java.util.UUID

import models._
import slick.driver.PostgresDriver
import slick.driver.PostgresDriver.api._
import store._
import org.joda.time.DateTime
import models.LwmDateTime._

import scala.concurrent.Future

case class TimetableLabworkFilter(value: String) extends TableFilter[TimetableTable] {
  override def predicate = _.labwork === UUID.fromString(value)
}

trait TimetableService2 extends AbstractDao[TimetableTable, TimetableDb, Timetable] {
  import scala.concurrent.ExecutionContext.Implicits.global

  override val tableQuery = TableQuery[TimetableTable]
  protected val timetableBlacklistQuery: TableQuery[TimetableBlacklistTable] = TableQuery[TimetableBlacklistTable]
  protected val timetableEntryQuery: TableQuery[TimetableEntryTable] = TableQuery[TimetableEntryTable]
  protected val timetableEntrySupervisorQuery: TableQuery[TimetableEntrySupervisorTable] = TableQuery[TimetableEntrySupervisorTable]

  override protected def toAtomic(query: Query[TimetableTable, TimetableDb, Seq]): Future[Seq[Timetable]] = collectDependencies(query) {
    case (timetable, labwork, blacklists, entries) =>
      val timetableEntries = entries.map {
        case ((e, r), s) => PostgresTimetableEntryAtom(s.map(_.toUser).toSet, r.toRoom, e.dayIndex, e.start.localTime, e.end.localTime)
      }

      PostgresTimetableAtom(labwork.toLabwork, timetableEntries.toSet, timetable.start.localDate, blacklists.map(_.toBlacklist).toSet, timetable.id)
  }

  override protected def toUniqueEntity(query: Query[TimetableTable, TimetableDb, Seq]): Future[Seq[Timetable]] = collectDependencies(query) {
    case (timetable, labwork, blacklists, entries) =>
      val timetableEntries = entries.map {
        case ((e, _), s) => PostgresTimetableEntry(s.map(_.id).toSet, e.room, e.dayIndex, e.start.localTime, e.end.localTime)
      }

      PostgresTimetable(labwork.id, timetableEntries.toSet, timetable.start.localDate, blacklists.map(_.toBlacklist).toSet, timetable.id)
  }

  private final def collectDependencies(query: Query[TimetableTable, TimetableDb, Seq])
                                       (build: (TimetableDb, LabworkDb, Seq[BlacklistDb], Map[(TimetableEntryDb, RoomDb), Seq[DbUser]]) => Timetable) = {
    val mandatory = for {
      q <- query
      l <- q.joinLabwork
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

  override protected def setInvalidated(entity: TimetableDb): TimetableDb = {
    val now = DateTime.now.timestamp

    entity.copy(lastModified = now, invalidated = Some(now))
  }

  override protected def existsQuery(entity: TimetableDb): Query[TimetableTable, TimetableDb, Seq] = {
    filterBy(List(TimetableLabworkFilter(entity.labwork.toString)))
  }

  override protected def shouldUpdate(existing: TimetableDb, toUpdate: TimetableDb): Boolean = {
    (!existing.start.equals(toUpdate.start) ||
      existing.entries != toUpdate.entries ||
      existing.localBlacklist != toUpdate.localBlacklist) &&
      existing.labwork == toUpdate.labwork
  }

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
        _ <- timetableEntryQuery ++= timetableEntries
        _ <- timetableEntrySupervisorQuery ++= supervisors
        _ <- timetableBlacklistQuery ++= blacklists
      } yield entities
    }

    override def expandDeleteOf(entity: TimetableDb) = {
      val timetableEntries = timetableEntryQuery.filter(_.timetable === entity.id).map(_.id)

      val deleted = for {
        d1 <- timetableEntrySupervisorQuery.filter(_.timetableEntry in timetableEntries).delete
        d2 <- timetableEntryQuery.filter(_.id in timetableEntries).delete
        d3 <- timetableBlacklistQuery.filter(_.timetable === entity.id).delete
      } yield d1 + d2 + d3

      deleted.map(_ => Some(entity))
    }

    override def expandUpdateOf(entity: TimetableDb) = {
      for {
        d <- expandDeleteOf(entity) if d.isDefined
        c <- expandCreationOf(Seq(entity))
      } yield c.headOption
    }
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
