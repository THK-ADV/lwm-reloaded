package dao

import dao.helper.TableFilter
import database.{SemesterDb, SemesterTable}
import javax.inject.Inject
import models.Semester
import org.joda.time.LocalDate
import slick.jdbc.PostgresProfile
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.{ExecutionContext, Future}

object SemesterDao extends TableFilter[SemesterTable] {

  import utils.date.DateTimeOps.LocalDateConverter

  def currentFilter(now: LocalDate = LocalDate.now): TableFilterPredicate = {
    val sqlNow = now.sqlDate
    t => t.start <= sqlNow && t.end >= sqlNow
  }

  def startFilter(start: LocalDate): TableFilterPredicate = _.start === start.sqlDate

  def endFilter(end: LocalDate): TableFilterPredicate = _.end === end.sqlDate

  def sinceFilter(date: LocalDate): TableFilterPredicate = _.start >= date.sqlDate

  def untilFilter(date: LocalDate): TableFilterPredicate = _.end <= date.sqlDate
}

trait SemesterDao extends AbstractDao[SemesterTable, SemesterDb, Semester] {

  import SemesterDao.{currentFilter, endFilter, startFilter}
  import dao.helper.TableFilter.labelFilterEquals
  import utils.date.DateTimeOps.SqlDateConverter

  override val tableQuery: TableQuery[SemesterTable] = TableQuery[SemesterTable]

  final def current(atomic: Boolean) = getSingleWhere(currentFilter().apply)

  override protected def shouldUpdate(existing: SemesterDb, toUpdate: SemesterDb): Boolean = {
    existing.label == toUpdate.label &&
      existing.start.localDate == toUpdate.start.localDate &&
      existing.end.localDate == toUpdate.end.localDate
  }

  override protected def existsQuery(entity: SemesterDb): PostgresProfile.api.Query[SemesterTable, SemesterDb, Seq] = {
    filterBy(List(labelFilterEquals(entity.label), startFilter(entity.start.localDate), endFilter(entity.end.localDate)))
  }

  override protected def toAtomic(query: PostgresProfile.api.Query[SemesterTable, SemesterDb, Seq]): Future[Seq[Semester]] = toUniqueEntity(query)

  override protected def toUniqueEntity(query: PostgresProfile.api.Query[SemesterTable, SemesterDb, Seq]): Future[Seq[Semester]] = {
    db.run(query.result.map(_.map(_.toUniqueEntity)))
  }
}

final class SemesterDaoImpl @Inject()(val db: Database, val executionContext: ExecutionContext) extends SemesterDao