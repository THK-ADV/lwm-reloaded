package dao

import dao.helper.DatabaseExpander
import database._
import javax.inject.Inject
import models._
import slick.jdbc
import slick.jdbc.PostgresProfile.api._
import slick.lifted.TableQuery
import utils.date.DateTimeOps._

import scala.concurrent.{ExecutionContext, Future}

trait ReportCardRetryDao extends AbstractDao[ReportCardRetryTable, ReportCardRetryDb, ReportCardRetryLike] {

  import dao.helper.TableFilterable.reportCardEntryFilter

  override val tableQuery = TableQuery[ReportCardRetryTable]
  val entryTypeQuery: TableQuery[ReportCardEntryTypeTable] = TableQuery[ReportCardEntryTypeTable]

  override protected def toAtomic(query: Query[ReportCardRetryTable, ReportCardRetryDb, Seq]): Future[Seq[ReportCardRetryLike]] = collectDependencies(query) {
    case (entry, room, entryTypes) => ReportCardRetryAtom(
      entry.date.localDate,
      entry.start.localTime,
      entry.end.localTime,
      room.toUniqueEntity,
      entryTypes.map(_.toUniqueEntity).toSet,
      entry.reason,
      entry.id
    )
  }

  override protected def toUniqueEntity(query: Query[ReportCardRetryTable, ReportCardRetryDb, Seq]): Future[Seq[ReportCardRetryLike]] = collectDependencies(query) {
    case (entry, _, entryTypes) => entry.copy(entryTypes = entryTypes.toSet).toUniqueEntity
  }

  private def collectDependencies(query: Query[ReportCardRetryTable, ReportCardRetryDb, Seq])
    (build: (ReportCardRetryDb, RoomDb, Seq[ReportCardEntryTypeDb]) => ReportCardRetryLike) = {
    val mandatory = for {
      q <- query
      r <- q.roomFk
    } yield (q, r)

    val action = mandatory.joinLeft(entryTypeQuery).on(_._1.id === _.reportCardRetry).result.map(_.groupBy(_._1._1.id).map {
      case (id, dependencies) =>
        val ((retry, room), _) = dependencies.find(_._1._1.id == id).get // lhs
      val entryTypes = dependencies.flatMap(_._2) // rhs

        build(retry, room, entryTypes)
    }.toSeq)

    db.run(action)
  }

  override protected def existsQuery(entity: ReportCardRetryDb): Query[ReportCardRetryTable, ReportCardRetryDb, Seq] = {
    filterBy(List(reportCardEntryFilter(entity.reportCardEntry)))
  }

  override protected def shouldUpdate(existing: ReportCardRetryDb, toUpdate: ReportCardRetryDb): Boolean = {
    import utils.date.DateTimeOps.{SqlDateConverter, SqlTimeConverter}

    (existing.date.localDate != toUpdate.date.localDate ||
      existing.start.localTime != toUpdate.start.localTime ||
      existing.end.localTime != toUpdate.end.localTime ||
      existing.reason != toUpdate.reason ||
      existing.room != toUpdate.room ||
      existing.entryTypes != toUpdate.entryTypes) &&
      existing.reportCardEntry == toUpdate.reportCardEntry
  }

  override protected val databaseExpander: Option[DatabaseExpander[ReportCardRetryDb]] = Some(new DatabaseExpander[ReportCardRetryDb] {

    override def expandCreationOf[E <: Effect](entities: ReportCardRetryDb*): jdbc.PostgresProfile.api.DBIOAction[Seq[ReportCardRetryDb], jdbc.PostgresProfile.api.NoStream, Effect.Write with Any] = for {
      _ <- entryTypeQuery ++= entities.flatMap(_.entryTypes)
    } yield entities

    override def expandDeleteOf(entity: ReportCardRetryDb) = for {
      _ <- entryTypeQuery.filter(_.reportCardRetry === entity.id).delete
    } yield entity

    override def expandUpdateOf(entity: ReportCardRetryDb) = for {
      d <- expandDeleteOf(entity)
      c <- expandCreationOf(d)
    } yield c.head
  })
}

final class ReportCardRetryDaoImpl @Inject()(val db: Database, val executionContext: ExecutionContext) extends ReportCardRetryDao