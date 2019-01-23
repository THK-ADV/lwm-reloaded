package dao

import java.util.UUID

import javax.inject.Inject
import models._
import slick.jdbc.PostgresProfile
import slick.jdbc.PostgresProfile.api._
import database.{LabworkDb, LabworkTable, TableFilter}

import scala.concurrent.Future

case class LabworkIdFilter(value: String) extends TableFilter[LabworkTable] {
  override def predicate = _.id === UUID.fromString(value)
}

case class LabworkDegreeFilter(value: String) extends TableFilter[LabworkTable] {
  override def predicate = _.degree === UUID.fromString(value)
}

case class LabworkSemesterFilter(value: String) extends TableFilter[LabworkTable] {
  override def predicate = _.semester === UUID.fromString(value)
}

case class LabworkCourseFilter(value: String) extends TableFilter[LabworkTable] {
  override def predicate = _.course === UUID.fromString(value)
}

case class LabworkLabelFilter(value: String) extends TableFilter[LabworkTable] {
  override def predicate = _.label.toLowerCase like s"%${value.toLowerCase}%"
}

case class LabworkSubscribableFilter(value: String) extends TableFilter[LabworkTable] {
  override def predicate = _.subscribable === value.toBoolean
}

case class LabworkPublishedFilter(value: String) extends TableFilter[LabworkTable] {
  override def predicate = _.published === value.toBoolean
}

trait LabworkDao extends AbstractDao[LabworkTable, LabworkDb, LabworkLike] {

  import scala.concurrent.ExecutionContext.Implicits.global

  override val tableQuery: TableQuery[LabworkTable] = TableQuery[LabworkTable]

  override protected def shouldUpdate(existing: LabworkDb, toUpdate: LabworkDb): Boolean = {
    (existing.label != toUpdate.label ||
      existing.description != toUpdate.description ||
      existing.subscribable != toUpdate.subscribable ||
      existing.published != toUpdate.published) &&
      (existing.semester == toUpdate.semester && existing.course == toUpdate.course && existing.degree == toUpdate.degree)
  }

  override protected def existsQuery(entity: LabworkDb): Query[LabworkTable, LabworkDb, Seq] = {
    filterBy(List(
      LabworkSemesterFilter(entity.semester.toString),
      LabworkCourseFilter(entity.course.toString),
      LabworkDegreeFilter(entity.degree.toString)
    ))
  }

  override protected def toAtomic(query: Query[LabworkTable, LabworkDb, Seq]): Future[Seq[LabworkLike]] = {
    val joinedQuery = for {
      q <- query
      c <- q.courseFk
      s <- q.semesterFk
      d <- q.degreeFk
      l <- c.lecturerFk
    } yield (q, c, s, d, l)

    db.run(joinedQuery.result.map(_.map {
      case (l, c, s, d, u) =>
        val courseAtom = CourseAtom(c.label, c.description, c.abbreviation, u.toUniqueEntity, c.semesterIndex, c.id)
        LabworkAtom(l.label, l.description, s.toUniqueEntity, courseAtom, d.toUniqueEntity, l.subscribable, l.published, l.id)
    }))
  }

  override protected def toUniqueEntity(query: Query[LabworkTable, LabworkDb, Seq]): Future[Seq[LabworkLike]] = {
    db.run(query.result.map(_.map(_.toUniqueEntity)))
  }
}

final class LabworkDaoImpl @Inject()(val db: PostgresProfile.backend.Database) extends LabworkDao