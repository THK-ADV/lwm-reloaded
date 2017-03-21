package services

import models.{DegreeDb, PostgresDegree}
import org.joda.time.DateTime
import slick.lifted.Rep
import store.{DegreeTable, PostgresDatabase, TableFilter}
import slick.driver.PostgresDriver.api._

import scala.concurrent.Future

case class DegreeAbbreviationFilter(value: String) extends TableFilter[DegreeTable] {
  override def predicate: (DegreeTable) => Rep[Boolean] = _.abbreviation.toLowerCase === value.toLowerCase
}

trait DegreeService extends AbstractDao[DegreeTable, DegreeDb, PostgresDegree] { self: PostgresDatabase =>
  import scala.concurrent.ExecutionContext.Implicits.global

  override val tableQuery: TableQuery[DegreeTable] = TableQuery[DegreeTable]

  override protected def setInvalidated(entity: DegreeDb): DegreeDb = {
    DegreeDb(entity.label, entity.abbreviation, Some(DateTime.now), entity.id)
  }

  override protected def toAtomic(query: Query[DegreeTable, DegreeDb, Seq]): Future[Seq[PostgresDegree]] = toUniqueEntity(query)

  override protected def toUniqueEntity(query: Query[DegreeTable, DegreeDb, Seq]): Future[Seq[PostgresDegree]] = db.run(query.result.map(_.map(_.toDegree)))
}

object DegreeService extends DegreeService with PostgresDatabase