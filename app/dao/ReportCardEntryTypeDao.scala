package dao

import java.util.UUID

import database.{ReportCardEntryTypeDb, ReportCardEntryTypeTable, TableFilter}
import javax.inject.Inject
import models.ReportCardEntryType
import org.joda.time.DateTime
import slick.jdbc.PostgresProfile
import slick.jdbc.PostgresProfile.api._
import slick.lifted.TableQuery

import scala.concurrent.{ExecutionContext, Future}

case class ReportCardEntryTypeLabelFilter(value: String) extends TableFilter[ReportCardEntryTypeTable] {
  override def predicate = _.entryType === value
}

trait ReportCardEntryTypeDao extends AbstractDao[ReportCardEntryTypeTable, ReportCardEntryTypeDb, ReportCardEntryType] {

  import utils.LwmDateTime._

  override val tableQuery = TableQuery[ReportCardEntryTypeTable]

  override protected def toAtomic(query: Query[ReportCardEntryTypeTable, ReportCardEntryTypeDb, Seq]): Future[Seq[ReportCardEntryType]] = toUniqueEntity(query)

  override protected def toUniqueEntity(query: Query[ReportCardEntryTypeTable, ReportCardEntryTypeDb, Seq]): Future[Seq[ReportCardEntryType]] = {
    db.run(query.result.map(_.map(_.toUniqueEntity)))
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

  def updateFields(id: UUID, bool: Option[Boolean], int: Int): Future[Int] = db.run(
    filterValidOnly(_.id === id).map(f => (f.bool, f.int, f.lastModified)).update((bool, int, DateTime.now.timestamp))
  )
}

final class ReportCardEntryTypeDaoImpl @Inject()(val db: PostgresProfile.backend.Database, val executionContext: ExecutionContext) extends ReportCardEntryTypeDao