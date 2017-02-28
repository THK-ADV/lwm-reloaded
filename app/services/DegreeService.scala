package services

import models.PostgresDegree
import slick.lifted.Rep
import store.{DegreeTable, TableFilter}
import slick.driver.PostgresDriver.api._

import scala.concurrent.Future

case class DegreeAbbreviationFilter(value: String) extends TableFilter[DegreeTable] {
  override def predicate: (DegreeTable) => Rep[Boolean] = _.abbreviation.toLowerCase === value.toLowerCase
}

trait DegreeService extends AbstractDao[DegreeTable, PostgresDegree, PostgresDegree] {

  override protected def tableQuery: TableQuery[DegreeTable] = TableQuery[DegreeTable]

  override protected def toAtomic(query: Query[DegreeTable, PostgresDegree, Seq]): Future[Seq[PostgresDegree]] = toUniqueEntity(query)

  override protected def toUniqueEntity(query: Query[DegreeTable, PostgresDegree, Seq]): Future[Seq[PostgresDegree]] = db.run(query.result)
}

object DegreeService extends DegreeService