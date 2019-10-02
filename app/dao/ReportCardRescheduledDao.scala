package dao

import dao.helper.TableFilter
import database.{ReportCardRescheduledDb, ReportCardRescheduledTable}
import javax.inject.Inject
import models._
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.{ExecutionContext, Future}

trait ReportCardRescheduledDao extends AbstractDao[ReportCardRescheduledTable, ReportCardRescheduledDb, ReportCardRescheduledLike] {

  import TableFilter.reportCardEntryFilter
  import utils.date.DateTimeOps._

  override val tableQuery = TableQuery[ReportCardRescheduledTable]

  override protected def toAtomic(query: Query[ReportCardRescheduledTable, ReportCardRescheduledDb, Seq]): Future[Seq[ReportCardRescheduledLike]] = {
    val mandatory = for {
      q <- query
      r <- q.roomFk
    } yield (q, r)

    db.run(mandatory.result.map(_.map {
      case (entry, room) => ReportCardRescheduledAtom(entry.date.localDate, entry.start.localTime, entry.end.localTime, room.toUniqueEntity, entry.reason, entry.id)
    }))
  }

  override protected def toUniqueEntity(query: Query[ReportCardRescheduledTable, ReportCardRescheduledDb, Seq]): Future[Seq[ReportCardRescheduledLike]] = {
    db.run(query.result.map(_.map(_.toUniqueEntity)))
  }

  override protected def existsQuery(entity: ReportCardRescheduledDb): Query[ReportCardRescheduledTable, ReportCardRescheduledDb, Seq] = {
    filterBy(List(reportCardEntryFilter(entity.reportCardEntry)))
  }

  override protected def shouldUpdate(existing: ReportCardRescheduledDb, toUpdate: ReportCardRescheduledDb): Boolean = {
    existing.reportCardEntry == toUpdate.reportCardEntry
  }
}

final class ReportCardRescheduledDaoImpl @Inject()(val db: Database, val executionContext: ExecutionContext) extends ReportCardRescheduledDao
