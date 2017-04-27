package services

import java.sql.Date
import java.util.UUID

import models.{PostgresSemester, SemesterDb}
import org.joda.time.DateTime
import store.{PostgresDatabase, SemesterTable, TableFilter}
import slick.driver.PostgresDriver.api._

import scala.concurrent.Future
import models.LwmDateTime.DateTimeConverter
import slick.driver.PostgresDriver

case class SemesterLabelFilter(value: String) extends TableFilter[SemesterTable] {
  override def predicate = _.label.toLowerCase like s"%${value.toLowerCase}%"
}

case class SemesterAbbreviationFilter(value: String) extends TableFilter[SemesterTable] {
  override def predicate = _.abbreviation.toLowerCase like s"%${value.toLowerCase}%"
}

case class SemesterStartFilter(value: String) extends TableFilter[SemesterTable] {
  override def predicate = _.start === Date.valueOf(value)
}

case class SemesterEndFilter(value: String) extends TableFilter[SemesterTable] {
  override def predicate = _.end === Date.valueOf(value)
}

case class SemesterCurrentFilter(value: String) extends TableFilter[SemesterTable] {
  override def predicate = t => t.start <= Date.valueOf(value) && t.end >= Date.valueOf(value)
}

case class SemesterIdFilter(value: String) extends TableFilter[SemesterTable] {
  override def predicate = _.id === UUID.fromString(value)
}

trait SemesterService extends AbstractDao[SemesterTable, SemesterDb, PostgresSemester] {
  import scala.concurrent.ExecutionContext.Implicits.global

  override val tableQuery: TableQuery[SemesterTable] = TableQuery[SemesterTable]

  override protected def setInvalidated(entity: SemesterDb): SemesterDb = {
    val now = DateTime.now.timestamp

    SemesterDb(
      entity.label,
      entity.abbreviation,
      entity.start,
      entity.end,
      entity.examStart,
      now,
      Some(now),
      entity.id
    )
  }

  override protected def shouldUpdate(existing: SemesterDb, toUpdate: SemesterDb): Boolean = {
    (existing.abbreviation != toUpdate.abbreviation ||
      !existing.examStart.equals(toUpdate.examStart)) &&
      (existing.label == toUpdate.label && existing.start.equals(toUpdate.start) && existing.end.equals(toUpdate.end))
  }

  override protected def existsQuery(entity: SemesterDb): Query[SemesterTable, SemesterDb, Seq] = {
    filterBy(List(
      SemesterLabelFilter(entity.label),
      SemesterStartFilter(entity.start.toString),
      SemesterEndFilter(entity.end.toString)
    ))
  }

  override protected def toAtomic(query: Query[SemesterTable, SemesterDb, Seq]): Future[Seq[PostgresSemester]] = toUniqueEntity(query)

  override protected def toUniqueEntity(query: Query[SemesterTable, SemesterDb, Seq]): Future[Seq[PostgresSemester]] = {
    db.run(query.result.map(_.map(_.toSemester)))
  }
}

final class SemesterServiceImpl(val db: PostgresDriver.backend.Database) extends SemesterService
