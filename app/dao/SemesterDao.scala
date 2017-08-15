package dao

import java.util.UUID

import models.LwmDateTime._
import models.{PostgresSemester, SemesterDb}
import slick.driver.PostgresDriver
import slick.driver.PostgresDriver.api._
import store.{SemesterTable, TableFilter}

import scala.concurrent.Future

case class SemesterLabelFilter(value: String) extends TableFilter[SemesterTable] {
  override def predicate = _.label.toLowerCase like s"%${value.toLowerCase}%"
}

case class SemesterAbbreviationFilter(value: String) extends TableFilter[SemesterTable] {
  override def predicate = _.abbreviation.toLowerCase like s"%${value.toLowerCase}%"
}

case class SemesterStartFilter(value: String) extends TableFilter[SemesterTable] {
  override def predicate = _.start === value.sqlDateFromMillis
}

case class SemesterSinceFilter(value: String) extends TableFilter[SemesterTable] {
  override def predicate = _.start >= value.sqlDateFromMillis
}

case class SemesterUntilFilter(value: String) extends TableFilter[SemesterTable] {
  override def predicate = _.end <= value.sqlDateFromMillis
}

case class SemesterEndFilter(value: String) extends TableFilter[SemesterTable] {
  override def predicate = _.end === value.sqlDateFromMillis
}

case class SemesterCurrentFilter(value: String) extends TableFilter[SemesterTable] {
  override def predicate = t => t.start <= value.sqlDateFromMillis && t.end >= value.sqlDateFromMillis
}

case class SemesterIdFilter(value: String) extends TableFilter[SemesterTable] {
  override def predicate = _.id === UUID.fromString(value)
}

trait SemesterDao extends AbstractDao[SemesterTable, SemesterDb, PostgresSemester] {
  import scala.concurrent.ExecutionContext.Implicits.global

  override val tableQuery: TableQuery[SemesterTable] = TableQuery[SemesterTable]

  override protected def shouldUpdate(existing: SemesterDb, toUpdate: SemesterDb): Boolean = {
    (existing.abbreviation != toUpdate.abbreviation ||
      !existing.examStart.equals(toUpdate.examStart)) &&
      (existing.label == toUpdate.label && existing.start.equals(toUpdate.start) && existing.end.equals(toUpdate.end))
  }

  override protected def existsQuery(entity: SemesterDb): Query[SemesterTable, SemesterDb, Seq] = {
    filterBy(List(
      SemesterLabelFilter(entity.label),
      SemesterStartFilter(entity.start.stringMillis),
      SemesterEndFilter(entity.end.stringMillis)
    ))
  }

  override protected def toAtomic(query: Query[SemesterTable, SemesterDb, Seq]): Future[Seq[PostgresSemester]] = toUniqueEntity(query)

  override protected def toUniqueEntity(query: Query[SemesterTable, SemesterDb, Seq]): Future[Seq[PostgresSemester]] = {
    db.run(query.result.map(_.map(_.toLwmModel)))
  }
}

final class SemesterDaoImpl(val db: PostgresDriver.backend.Database) extends SemesterDao
