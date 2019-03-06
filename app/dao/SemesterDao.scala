package dao

import java.util.UUID

import javax.inject.{Inject, Singleton}
import models.Semester
import org.joda.time.LocalDate
import slick.jdbc.PostgresProfile
import slick.jdbc.PostgresProfile.api._
import database.{SemesterDb, SemesterTable, TableFilter}
import utils.LwmDateTime._

import scala.concurrent.{ExecutionContext, Future}

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

case object SemesterCurrentFilter extends TableFilter[SemesterTable] {
  override def predicate = t => t.start <= value.sqlDateFromMillis && t.end >= value.sqlDateFromMillis
  override def value: String = LocalDate.now.stringMillis
}

case class SemesterIdFilter(value: String) extends TableFilter[SemesterTable] {
  override def predicate = _.id === UUID.fromString(value)
}

trait SemesterDao extends AbstractDao[SemesterTable, SemesterDb, Semester] {

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

  override protected def toAtomic(query: PostgresProfile.api.Query[SemesterTable, SemesterDb, Seq]): Future[Seq[Semester]] = toUniqueEntity(query)

  override protected def toUniqueEntity(query: PostgresProfile.api.Query[SemesterTable, SemesterDb, Seq]): Future[Seq[Semester]] = {
    db.run(query.result.map(_.map(_.toUniqueEntity)))
  }
}

final class SemesterDaoImpl @Inject() (val db: PostgresProfile.backend.Database, val executionContext: ExecutionContext) extends SemesterDao