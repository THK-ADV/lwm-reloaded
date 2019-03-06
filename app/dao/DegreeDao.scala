package dao

import java.util.UUID

import javax.inject.Inject
import models.Degree
import slick.jdbc.PostgresProfile
import slick.jdbc.PostgresProfile.api._
import database.{DegreeDb, DegreeTable, TableFilter}

import scala.concurrent.{ExecutionContext, Future}

case class DegreeAbbreviationFilter(value: String) extends TableFilter[DegreeTable] {
  override def predicate = _.abbreviation.toLowerCase === value.toLowerCase
}

case class DegreeLabelFilter(value: String) extends TableFilter[DegreeTable] {
  override def predicate = _.label.toLowerCase like s"%${value.toLowerCase}%"
}

case class DegreeIdFilter(value: String) extends TableFilter[DegreeTable] {
  override def predicate = _.id === UUID.fromString(value)
}

trait DegreeDao extends AbstractDao[DegreeTable, DegreeDb, Degree] {

  override val tableQuery: TableQuery[DegreeTable] = TableQuery[DegreeTable]

  override protected def shouldUpdate(existing: DegreeDb, toUpdate: DegreeDb): Boolean = {
    existing.label != toUpdate.label && existing.abbreviation == toUpdate.abbreviation
  }

  override protected def existsQuery(entity: DegreeDb): Query[DegreeTable, DegreeDb, Seq] = {
    filterBy(List(DegreeAbbreviationFilter(entity.abbreviation)))
  }

  override protected def toAtomic(query: Query[DegreeTable, DegreeDb, Seq]): Future[Seq[Degree]] = toUniqueEntity(query)

  override protected def toUniqueEntity(query: Query[DegreeTable, DegreeDb, Seq]): Future[Seq[Degree]] = db.run(query.result.map(_.map(_.toUniqueEntity)))
}

final class DegreeDaoImpl @Inject()(val db: PostgresProfile.backend.Database, val executionContext: ExecutionContext) extends DegreeDao