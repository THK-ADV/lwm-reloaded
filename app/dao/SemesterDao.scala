package dao

import java.util.UUID

import database.{SemesterDb, SemesterTable, TableFilter}
import javax.inject.Inject
import models.Semester
import org.joda.time.LocalDate
import slick.jdbc.PostgresProfile
import slick.jdbc.PostgresProfile.api._
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

  final def current(atomic: Boolean) = getSingleWhere(SemesterCurrentFilter.predicate)

  override protected def shouldUpdate(existing: SemesterDb, toUpdate: SemesterDb): Boolean = {
    import utils.LwmDateTime.SqlDateConverter

    (existing.abbreviation != toUpdate.abbreviation ||
      existing.examStart.localDate != toUpdate.examStart.localDate) &&
      (existing.label == toUpdate.label && existing.start.localDate == toUpdate.start.localDate && existing.end.localDate == toUpdate.end.localDate)
  }

  override protected def existsQuery(entity: SemesterDb): PostgresProfile.api.Query[SemesterTable, SemesterDb, Seq] = {
    filterBy(List(
      SemesterLabelFilter(entity.label),
      SemesterStartFilter(entity.start.stringMillis),
      SemesterEndFilter(entity.end.stringMillis)
    ))
  }

  override protected def toAtomic(query: PostgresProfile.api.Query[SemesterTable, SemesterDb, Seq]): Future[Traversable[Semester]] = toUniqueEntity(query)

  override protected def toUniqueEntity(query: PostgresProfile.api.Query[SemesterTable, SemesterDb, Seq]): Future[Traversable[Semester]] = {
    db.run(query.result.map(_.map(_.toUniqueEntity)))
  }
}

final class SemesterDaoImpl @Inject()(val db: Database, val executionContext: ExecutionContext) extends SemesterDao