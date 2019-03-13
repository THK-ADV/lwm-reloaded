package dao

import java.util.UUID

import database.{ReportCardRescheduledDb, ReportCardRescheduledTable, TableFilter}
import javax.inject.Inject
import models._
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.{ExecutionContext, Future}

case class ReportCardRescheduledEntryFilter(value: String) extends TableFilter[ReportCardRescheduledTable] {
  override def predicate = _.reportCardEntry === UUID.fromString(value)
}

case class ReportCardRescheduledLabworkFilter(value: String) extends TableFilter[ReportCardRescheduledTable] {
  override def predicate = _.reportCardEntryFk.filter(_.labwork === UUID.fromString(value)).exists
}

case class ReportCardRescheduledCourseFilter(value: String) extends TableFilter[ReportCardRescheduledTable] {
  override def predicate = _.reportCardEntryFk.map(_.memberOfCourse(value)).exists
}

trait ReportCardRescheduledDao extends AbstractDao[ReportCardRescheduledTable, ReportCardRescheduledDb, ReportCardRescheduledLike] {

  import utils.LwmDateTime._

  override val tableQuery = TableQuery[ReportCardRescheduledTable]

  override protected def toAtomic(query: Query[ReportCardRescheduledTable, ReportCardRescheduledDb, Seq]): Future[Traversable[ReportCardRescheduledLike]] = {
    val mandatory = for {
      q <- query
      r <- q.roomFk
    } yield (q, r)

    db.run(mandatory.result.map(_.map {
      case (entry, room) => ReportCardRescheduledAtom(entry.date.localDate, entry.start.localTime, entry.end.localTime, room.toUniqueEntity, entry.reason, entry.id)
    }))
  }

  override protected def toUniqueEntity(query: Query[ReportCardRescheduledTable, ReportCardRescheduledDb, Seq]): Future[Traversable[ReportCardRescheduledLike]] = {
    db.run(query.result.map(_.map(_.toUniqueEntity)))
  }

  override protected def existsQuery(entity: ReportCardRescheduledDb): Query[ReportCardRescheduledTable, ReportCardRescheduledDb, Seq] = {
    filterBy(List(ReportCardRescheduledEntryFilter(entity.reportCardEntry.toString)))
  }

  override protected def shouldUpdate(existing: ReportCardRescheduledDb, toUpdate: ReportCardRescheduledDb): Boolean = {
    import utils.LwmDateTime.{SqlDateConverter, TimeConverter}

    (existing.date.localDate != toUpdate.date.localDate ||
      existing.start.localTime != toUpdate.start.localTime ||
      existing.end.localTime != toUpdate.end.localTime ||
      existing.reason != toUpdate.reason ||
      existing.room != toUpdate.room) &&
      existing.reportCardEntry == toUpdate.reportCardEntry
  }
}

final class ReportCardRescheduledDaoImpl @Inject()(val db: Database, val executionContext: ExecutionContext) extends ReportCardRescheduledDao
