package dao

import java.util.UUID

import javax.inject.{Inject, Singleton}
import models.{PostgresSemester, SemesterDb}
import org.joda.time.LocalDate
import slick.jdbc.PostgresProfile
import slick.jdbc.PostgresProfile.api._
import store.{SemesterTable, TableFilter}
import utils.LwmDateTime._

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

case class SemesterCurrentFilter(value: String = LocalDate.now.stringMillis) extends TableFilter[SemesterTable] {
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

  override protected def existsQuery(entity: SemesterDb): PostgresProfile.api.Query[SemesterTable, SemesterDb, Seq] = {
    filterBy(List(
      SemesterLabelFilter(entity.label),
      SemesterStartFilter(entity.start.stringMillis),
      SemesterEndFilter(entity.end.stringMillis)
    ))
  }

  override protected def toAtomic(query: PostgresProfile.api.Query[SemesterTable, SemesterDb, Seq]): Future[Seq[PostgresSemester]] = toUniqueEntity(query)

  override protected def toUniqueEntity(query: PostgresProfile.api.Query[SemesterTable, SemesterDb, Seq]): Future[Seq[PostgresSemester]] = {
    db.run(query.result.map(_.map(_.toLwmModel)))
  }
}

@Singleton
final class SemesterDaoImpl @Inject() (val db: PostgresProfile.backend.Database) extends SemesterDao