package dao

import java.util.UUID

import models.{DegreeDb, PostgresDegree}
import slick.driver.PostgresDriver
import slick.driver.PostgresDriver.api._
import slick.lifted.Rep
import store.{DegreeTable, TableFilter}

import scala.concurrent.Future

case class DegreeAbbreviationFilter(value: String) extends TableFilter[DegreeTable] {
  override def predicate: (DegreeTable) => Rep[Boolean] = _.abbreviation.toLowerCase === value.toLowerCase
}
case class DegreeLabelFilter(value: String) extends TableFilter[DegreeTable] {
  override def predicate: (DegreeTable) => Rep[Boolean] = _.label.toLowerCase like s"%${value.toLowerCase}%"
}
case class DegreeIdFilter(value: String) extends TableFilter[DegreeTable] {
  override def predicate: (DegreeTable) => Rep[Boolean] = _.id === UUID.fromString(value)
}

trait DegreeDao extends AbstractDao[DegreeTable, DegreeDb, PostgresDegree] {
  import scala.concurrent.ExecutionContext.Implicits.global

  override val tableQuery: TableQuery[DegreeTable] = TableQuery[DegreeTable]

  override protected def shouldUpdate(existing: DegreeDb, toUpdate: DegreeDb): Boolean = {
    existing.label != toUpdate.label && existing.abbreviation == toUpdate.abbreviation
  }

  override protected def existsQuery(entity: DegreeDb): Query[DegreeTable, DegreeDb, Seq] = {
    filterBy(List(DegreeAbbreviationFilter(entity.abbreviation)))
  }

  override protected def toAtomic(query: Query[DegreeTable, DegreeDb, Seq]): Future[Seq[PostgresDegree]] = toUniqueEntity(query)

  override protected def toUniqueEntity(query: Query[DegreeTable, DegreeDb, Seq]): Future[Seq[PostgresDegree]] = db.run(query.result.map(_.map(_.toLwmModel)))
}

final class DegreeDaoImpl(val db: PostgresDriver.backend.Database) extends DegreeDao