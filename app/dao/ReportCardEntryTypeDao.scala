package dao

import java.util.UUID

import dao.helper.TableFilterable
import database.{ReportCardEntryTypeDb, ReportCardEntryTypeTable}
import javax.inject.Inject
import models.ReportCardEntryType
import org.joda.time.DateTime
import slick.jdbc.PostgresProfile.api._
import slick.lifted.TableQuery

import scala.concurrent.{ExecutionContext, Future}

object ReportCardEntryTypeDao extends TableFilterable[ReportCardEntryTypeTable] {
  def reportCardEntryFilter(reportCardEntry: UUID): TableFilterPredicate = _.reportCardEntryFk.filter(_.id === reportCardEntry).exists

  def reportCardRetryFilter(reportCardRetry: UUID): TableFilterPredicate = _.reportCardRetryFk.filter(_.id === reportCardRetry).exists
}

trait ReportCardEntryTypeDao extends AbstractDao[ReportCardEntryTypeTable, ReportCardEntryTypeDb, ReportCardEntryType] {
  import ReportCardEntryTypeDao._
  import TableFilterable.entryTypeFilter
  import utils.date.DateTimeOps._

  override val tableQuery = TableQuery[ReportCardEntryTypeTable]

  override protected def toAtomic(query: Query[ReportCardEntryTypeTable, ReportCardEntryTypeDb, Seq]): Future[Seq[ReportCardEntryType]] = toUniqueEntity(query)

  override protected def toUniqueEntity(query: Query[ReportCardEntryTypeTable, ReportCardEntryTypeDb, Seq]): Future[Seq[ReportCardEntryType]] = {
    db.run(query.result.map(_.map(_.toUniqueEntity)))
  }

  override protected def existsQuery(entity: ReportCardEntryTypeDb): Query[ReportCardEntryTypeTable, ReportCardEntryTypeDb, Seq] = {
    val labelFilter: List[TableFilterPredicate] = List(entryTypeFilter(entity.entryType))
    val withEntryFilter = entity.reportCardEntry.map(id => labelFilter.+:(reportCardEntryFilter(id)))
    val withRetryFilter = entity.reportCardRetry.map(id => labelFilter.+:(reportCardRetryFilter(id)))

    filterBy((withEntryFilter orElse withRetryFilter) getOrElse labelFilter)
  }

  override protected def shouldUpdate(existing: ReportCardEntryTypeDb, toUpdate: ReportCardEntryTypeDb): Boolean = {
    (existing.bool != toUpdate.bool ||
      existing.int != toUpdate.int ||
      existing.reportCardRetry != toUpdate.reportCardRetry) &&
      (existing.reportCardEntry == existing.reportCardEntry && existing.entryType == toUpdate.entryType)
  }

  def updateFields(id: UUID, bool: Option[Boolean], int: Int): Future[Int] = db.run(
    filterValidOnly(_.id === id).map(f => (f.bool, f.int, f.lastModified)).update((bool, int, DateTime.now.timestamp))
  )
}

final class ReportCardEntryTypeDaoImpl @Inject()(val db: Database, val executionContext: ExecutionContext) extends ReportCardEntryTypeDao