package dao

import java.util.UUID

import models.{PostgresReportCardEntryType, ReportCardEntryTypeDb}
import org.joda.time.DateTime
import slick.driver.PostgresDriver
import slick.driver.PostgresDriver.api._
import slick.lifted.TableQuery
import store.{ReportCardEntryTypeTable, TableFilter}

import scala.concurrent.Future

case class ReportCardEntryTypeLabelFilter(value: String) extends TableFilter[ReportCardEntryTypeTable] {
  override def predicate = _.entryType === value
}

trait ReportCardEntryTypeDao extends AbstractDao[ReportCardEntryTypeTable, ReportCardEntryTypeDb, PostgresReportCardEntryType] {

  import scala.concurrent.ExecutionContext.Implicits.global
  import models.LwmDateTime._

  override val tableQuery = TableQuery[ReportCardEntryTypeTable]

  override protected def toAtomic(query: Query[ReportCardEntryTypeTable, ReportCardEntryTypeDb, Seq]): Future[Seq[PostgresReportCardEntryType]] = toUniqueEntity(query)

  override protected def toUniqueEntity(query: Query[ReportCardEntryTypeTable, ReportCardEntryTypeDb, Seq]): Future[Seq[PostgresReportCardEntryType]] = {
    db.run(query.result.map(_.map(_.toLwmModel)))
  }

  override protected def existsQuery(entity: ReportCardEntryTypeDb): Query[ReportCardEntryTypeTable, ReportCardEntryTypeDb, Seq] = {
    filterBy(List(ReportCardEntryTypeLabelFilter(entity.entryType)))
  }

  override protected def shouldUpdate(existing: ReportCardEntryTypeDb, toUpdate: ReportCardEntryTypeDb): Boolean = {
    (existing.bool != toUpdate.bool ||
      existing.int != toUpdate.int ||
      existing.reportCardRetry != toUpdate.reportCardRetry) &&
      (existing.reportCardEntry == existing.reportCardEntry && existing.entryType == toUpdate.entryType)
  }

  def updateFields(id: UUID, bool: Option[Boolean], int: Int): Future[Boolean] = db.run(
    tableQuery.filter(_.id === id).map(f => (f.bool, f.int, f.lastModified)).update((bool, int, DateTime.now.timestamp)).map(_ > 0)
  )
}

final class ReportCardEntryTypeDaoImpl(val db: PostgresDriver.backend.Database) extends ReportCardEntryTypeDao