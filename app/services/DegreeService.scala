package services

import models.PostgresDegree
import store.DegreeTable

import scala.concurrent.Future

trait DegreeService extends AbstractDao[DegreeTable, PostgresDegree, PostgresDegree] {
  import slick.driver.PostgresDriver.api._

  override protected def tableQuery: TableQuery[DegreeTable] = TableQuery[DegreeTable]

  override protected def toAtomic(query: Query[DegreeTable, PostgresDegree, Seq]): Future[Seq[PostgresDegree]] = toUniqueEntity(query)

  override protected def toUniqueEntity(query: Query[DegreeTable, PostgresDegree, Seq]): Future[Seq[PostgresDegree]] = db.run(query.result)
}

object DegreeService extends DegreeService