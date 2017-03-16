package services

import models.{PostgresSemester, SemesterDb}
import store.{PostgresDatabase, SemesterTable}
import slick.driver.PostgresDriver.api._

import scala.concurrent.Future

trait SemesterService extends AbstractDao[SemesterTable, SemesterDb, PostgresSemester] { self: PostgresDatabase =>
  import scala.concurrent.ExecutionContext.Implicits.global

  override val tableQuery: TableQuery[SemesterTable] = TableQuery[SemesterTable]

  override protected def toAtomic(query: Query[SemesterTable, SemesterDb, Seq]): Future[Seq[PostgresSemester]] = toUniqueEntity(query)

  override protected def toUniqueEntity(query: Query[SemesterTable, SemesterDb, Seq]): Future[Seq[PostgresSemester]] = {
    db.run(query.result.map(_.map(_.toSemester)))
  }
}

object SemesterService extends SemesterService with PostgresDatabase
