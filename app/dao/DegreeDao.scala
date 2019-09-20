package dao

import java.util.UUID

import dao.helper.TableFilter
import database.{DegreeDb, DegreeTable}
import javax.inject.Inject
import models.Degree
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.{ExecutionContext, Future}

object DegreeDao extends TableFilter[DegreeTable] {
  def idFilter(id: UUID): TableFilterPredicate = TableFilter.idFilter(id)

  def labelFilter(label: String): TableFilterPredicate = TableFilter.labelFilterLike(label)

  def abbreviationFilter(abbreviation: String): TableFilterPredicate = TableFilter.abbreviationFilter(abbreviation)
}

trait DegreeDao extends AbstractDao[DegreeTable, DegreeDb, Degree] {

  import DegreeDao.abbreviationFilter

  override val tableQuery: TableQuery[DegreeTable] = TableQuery[DegreeTable]

  override protected def shouldUpdate(existing: DegreeDb, toUpdate: DegreeDb): Boolean = {
    existing.abbreviation == toUpdate.abbreviation
  }

  override protected def existsQuery(entity: DegreeDb): Query[DegreeTable, DegreeDb, Seq] = {
    filterBy(List(abbreviationFilter(entity.abbreviation)))
  }

  override protected def toAtomic(query: Query[DegreeTable, DegreeDb, Seq]): Future[Seq[Degree]] = toUniqueEntity(query)

  override protected def toUniqueEntity(query: Query[DegreeTable, DegreeDb, Seq]): Future[Seq[Degree]] = db.run(query.result.map(_.map(_.toUniqueEntity)))
}

final class DegreeDaoImpl @Inject()(val db: Database, val executionContext: ExecutionContext) extends DegreeDao