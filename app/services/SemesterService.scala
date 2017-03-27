package services

import java.util.UUID

import models.{LwmDateTime, PostgresSemester, SemesterDb}
import org.joda.time.{DateTime, LocalDate}
import store.{PostgresDatabase, SemesterTable, TableFilter}
import slick.driver.PostgresDriver.api._
import slick.lifted.Rep

import scala.concurrent.Future

case class SemesterLabelFilter(value: String) extends TableFilter[SemesterTable] {
  override def predicate = _.label.toLowerCase === value.toLowerCase
}

case class SemesterIdFilter(value: String) extends TableFilter[SemesterTable] {
  override def predicate = _.id === UUID.fromString(value)
}

trait SemesterService extends AbstractDao[SemesterTable, SemesterDb, PostgresSemester] { self: PostgresDatabase =>
  import scala.concurrent.ExecutionContext.Implicits.global

  override val tableQuery: TableQuery[SemesterTable] = TableQuery[SemesterTable]

  override protected def setInvalidated(entity: SemesterDb): SemesterDb = {
    SemesterDb(entity.label, entity.abbreviation, entity.start, entity.end, entity.examStart, Some(DateTime.now), entity.id)
  }

  override protected def shouldUpdate(existing: SemesterDb, toUpdate: SemesterDb): Boolean = {
    (existing.abbreviation != toUpdate.abbreviation ||
      !existing.examStart.isEqual(toUpdate.examStart)) &&
      existing.label == toUpdate.label
  }

  // TODO start, label, end
  override protected def existsQuery(entity: SemesterDb): Query[SemesterTable, SemesterDb, Seq] = {
    filterBy(List(SemesterLabelFilter(entity.label)))
  }

  override protected def toAtomic(query: Query[SemesterTable, SemesterDb, Seq]): Future[Seq[PostgresSemester]] = toUniqueEntity(query)

  override protected def toUniqueEntity(query: Query[SemesterTable, SemesterDb, Seq]): Future[Seq[PostgresSemester]] = {
    db.run(query.result.map(_.map(_.toSemester)))
  }
}

object SemesterService extends SemesterService with PostgresDatabase
