package dao

import java.util.UUID

import dao.helper.TableFilter
import database.{ReportCardEntryTypeDb, ReportCardEntryTypeTable}
import javax.inject.Inject
import models.ReportCardEntryType
import org.joda.time.DateTime
import slick.jdbc.PostgresProfile.api._
import slick.lifted.TableQuery

import scala.concurrent.{ExecutionContext, Future}

object ReportCardEntryTypeDao extends TableFilter[ReportCardEntryTypeTable] {
  def reportCardEntryFilter(reportCardEntry: UUID): TableFilterPredicate = _.reportCardEntryFk.filter(_.id === reportCardEntry).exists

  def reportCardRetryFilter(reportCardRetry: UUID): TableFilterPredicate = _.reportCardRetryFk.filter(_.id === reportCardRetry).exists
}

trait ReportCardEntryTypeDao extends AbstractDao[ReportCardEntryTypeTable, ReportCardEntryTypeDb, ReportCardEntryType] {
  def updateFields(id: UUID, bool: Option[Boolean], int: Int): Future[Int]

  def updateFields(users: List[UUID], assignmentEntry: UUID, labwork: UUID, entryType: String, bool: Boolean): Future[Int]

  def updateFields(users: List[UUID], assignmentEntry: UUID, labwork: UUID, entryType: String, int: Int): Future[Int]
}

final class ReportCardEntryTypeDaoImpl @Inject()(
  val db: Database,
  implicit val executionContext: ExecutionContext
) extends ReportCardEntryTypeDao {

  import ReportCardEntryTypeDao._
  import TableFilter.entryTypeFilter
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
    existing.entryType == toUpdate.entryType &&
      existing.reportCardEntry == toUpdate.reportCardEntry &&
      existing.reportCardRetry == toUpdate.reportCardRetry
  }

  def updateFields(id: UUID, bool: Option[Boolean], int: Int): Future[Int] = db.run(
    filterValidOnly(_.id === id).map(f => (f.bool, f.int, f.lastModified)).update((bool, int, DateTime.now.timestamp))
  )

  def updateFields(users: List[UUID], assignmentEntry: UUID, labwork: UUID, entryType: String, bool: Boolean): Future[Int] = {
    val update = updateWhere(users, assignmentEntry, labwork, entryType)
      .map(e => (e.bool, e.lastModified))
      .update((Some(bool), DateTime.now.timestamp))

    db.run(update)
  }

  private def updateWhere(users: List[UUID], assignmentEntry: UUID, labwork: UUID, entryType: String) =
    filterValidOnly { t =>
      t.joinReportCardEntry(e =>
        e.labwork === labwork &&
          e.joinAssignmentEntry(assignmentEntry).exists
          && e.user.inSet(users)
      ).exists &&
        t.entryType === entryType
    }

  def updateFields(users: List[UUID], assignmentEntry: UUID, labwork: UUID, entryType: String, int: Int): Future[Int] = {
    val update = updateWhere(users, assignmentEntry, labwork, entryType)
      .map(e => (e.int, e.lastModified))
      .update((int, DateTime.now.timestamp))

    db.run(update)
  }

}