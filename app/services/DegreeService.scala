package services

import java.util.UUID

import models.{DegreeDb, PostgresDegree}
import org.joda.time.DateTime
import slick.lifted.Rep
import store.{DegreeTable, PostgresDatabase, TableFilter}
import slick.driver.PostgresDriver.api._

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

trait DegreeService extends AbstractDao[DegreeTable, DegreeDb, PostgresDegree] { self: PostgresDatabase =>
  import scala.concurrent.ExecutionContext.Implicits.global

  override val tableQuery: TableQuery[DegreeTable] = TableQuery[DegreeTable]

  override protected def shouldUpdate(existing: DegreeDb, toUpdate: DegreeDb): Boolean = {
    existing.label != toUpdate.label && existing.abbreviation == toUpdate.abbreviation
  }

  override protected def existsQuery(entity: DegreeDb): Query[DegreeTable, DegreeDb, Seq] = {
    filterBy(List(DegreeAbbreviationFilter(entity.abbreviation)))
  }

  override protected def setInvalidated(entity: DegreeDb): DegreeDb = {
    DegreeDb(entity.label, entity.abbreviation, Some(DateTime.now), entity.id)
  }

  override protected def toAtomic(query: Query[DegreeTable, DegreeDb, Seq]): Future[Seq[PostgresDegree]] = toUniqueEntity(query)

  override protected def toUniqueEntity(query: Query[DegreeTable, DegreeDb, Seq]): Future[Seq[PostgresDegree]] = db.run(query.result.map(_.map(_.toDegree)))
}

object DegreeService extends DegreeService with PostgresDatabase