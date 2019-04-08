package dao

import database.{SemesterDb, SemesterTable}
import javax.inject.Inject
import models.Semester
import slick.jdbc.PostgresProfile
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.{ExecutionContext, Future}

//case class SemesterStartFilter(value: String) extends TableFilter[SemesterTable] { // TODO
//  override def predicate = _.start === value.sqlDateFromMillis
//}
//
//case class SemesterSinceFilter(value: String) extends TableFilter[SemesterTable] {
//  override def predicate = _.start >= value.sqlDateFromMillis
//}
//
//case class SemesterUntilFilter(value: String) extends TableFilter[SemesterTable] {
//  override def predicate = _.end <= value.sqlDateFromMillis
//}
//
//case class SemesterEndFilter(value: String) extends TableFilter[SemesterTable] {
//  override def predicate = _.end === value.sqlDateFromMillis
//}
//
//case object SemesterCurrentFilter extends TableFilter[SemesterTable] {
//  override def predicate = t => t.start <= value.sqlDateFromMillis && t.end >= value.sqlDateFromMillis
//
//  override def value: String = LocalDate.now.stringMillis
//}

trait SemesterDao extends AbstractDao[SemesterTable, SemesterDb, Semester] {
  import dao.helper.TableFilterable.labelFilterLike
  override val tableQuery: TableQuery[SemesterTable] = TableQuery[SemesterTable]

  final def current(atomic: Boolean) = Future.successful(Option.empty[Semester])//getSingleWhere(SemesterCurrentFilter.predicate) // TODO

  override protected def shouldUpdate(existing: SemesterDb, toUpdate: SemesterDb): Boolean = {
    import utils.LwmDateTime.SqlDateConverter

    (existing.abbreviation != toUpdate.abbreviation ||
      existing.examStart.localDate != toUpdate.examStart.localDate) &&
      (existing.label == toUpdate.label && existing.start.localDate == toUpdate.start.localDate && existing.end.localDate == toUpdate.end.localDate)
  }

  override protected def existsQuery(entity: SemesterDb): PostgresProfile.api.Query[SemesterTable, SemesterDb, Seq] = {
    filterBy(List(
      labelFilterLike(entity.label)/*,
      SemesterStartFilter(entity.start.stringMillis),
      SemesterEndFilter(entity.end.stringMillis)*/
    ))
  }

  override protected def toAtomic(query: PostgresProfile.api.Query[SemesterTable, SemesterDb, Seq]): Future[Seq[Semester]] = toUniqueEntity(query)

  override protected def toUniqueEntity(query: PostgresProfile.api.Query[SemesterTable, SemesterDb, Seq]): Future[Seq[Semester]] = {
    db.run(query.result.map(_.map(_.toUniqueEntity)))
  }
}

final class SemesterDaoImpl @Inject()(val db: Database, val executionContext: ExecutionContext) extends SemesterDao