package dao

import dao.helper.TableFilter
import database.{ReportCardEvaluationPatternDb, ReportCardEvaluationPatternTable}
import javax.inject.Inject
import models.ReportCardEvaluationPattern
import slick.jdbc.PostgresProfile.api._
import slick.lifted.TableQuery

import scala.concurrent.{ExecutionContext, Future}

object ReportCardEvaluationPatternDao extends TableFilter[ReportCardEvaluationPatternTable] {
  def propertyFilter(property: String): TableFilterPredicate = _.property.toLowerCase === property.toLowerCase
}

trait ReportCardEvaluationPatternDao extends AbstractDao[ReportCardEvaluationPatternTable, ReportCardEvaluationPatternDb, ReportCardEvaluationPattern] {

  import ReportCardEvaluationPatternDao._
  import TableFilter.{entryTypeFilter, labworkFilter}

  override val tableQuery = TableQuery[ReportCardEvaluationPatternTable]

  override protected def toAtomic(query: Query[ReportCardEvaluationPatternTable, ReportCardEvaluationPatternDb, Seq]): Future[Seq[ReportCardEvaluationPattern]] = toUniqueEntity(query)

  override protected def toUniqueEntity(query: Query[ReportCardEvaluationPatternTable, ReportCardEvaluationPatternDb, Seq]): Future[Seq[ReportCardEvaluationPattern]] = {
    db.run(query.result.map(_.map(_.toUniqueEntity)))
  }

  override protected def existsQuery(entity: ReportCardEvaluationPatternDb): Query[ReportCardEvaluationPatternTable, ReportCardEvaluationPatternDb, Seq] = {
    filterBy(List(propertyFilter(entity.property), entryTypeFilter(entity.entryType), labworkFilter(entity.labwork)))
  }

  override protected def shouldUpdate(existing: ReportCardEvaluationPatternDb, toUpdate: ReportCardEvaluationPatternDb): Boolean = {
    (existing.min != toUpdate.min) &&
      (existing.property == toUpdate.property && existing.entryType == toUpdate.entryType && existing.labwork == toUpdate.labwork)
  }
}

final class ReportCardEvaluationPatternDaoImpl @Inject()(val db: Database, val executionContext: ExecutionContext) extends ReportCardEvaluationPatternDao