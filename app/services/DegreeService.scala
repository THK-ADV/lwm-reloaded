package services

import models.PostgresDegree
import store.DegreeTable

trait DegreeService extends AbstractDao[DegreeTable, PostgresDegree] {
  import slick.driver.PostgresDriver.api._

  override protected def tableQuery: TableQuery[DegreeTable] = TableQuery[DegreeTable]
}

object DegreeService extends DegreeService