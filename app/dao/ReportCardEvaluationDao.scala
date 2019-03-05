package dao

import java.util.UUID

import javax.inject.Inject
import models._
import slick.jdbc.PostgresProfile
import slick.jdbc.PostgresProfile.api._
import slick.lifted.TableQuery
import database._
import utils.LwmDateTime.SqlTimestampConverter

import scala.concurrent.Future
import scala.util.Try

case class StudentFilter(value: String) extends TableFilter[ReportCardEvaluationTable] {
  override def predicate = _.student === UUID.fromString(value)
}

case class LabworkFilter(value: String) extends TableFilter[ReportCardEvaluationTable] {
  override def predicate = _.labwork === UUID.fromString(value)
}

case class CourseFilter(value: String) extends TableFilter[ReportCardEvaluationTable] {
  override def predicate = _.labworkFk.map(_.course).filter(_ === UUID.fromString(value)).exists
}

case class LabelFilter(value: String) extends TableFilter[ReportCardEvaluationTable] {
  override def predicate = _.label === value
}

case class BoolFilter(value: String) extends TableFilter[ReportCardEvaluationTable] {
  override def predicate = _.bool === Try(value.toBoolean).getOrElse(false)
}

case class IntFilter(value: String) extends TableFilter[ReportCardEvaluationTable] {
  override def predicate = _.int === Try(value.toInt).getOrElse(-1)
}

case class MinIntFilter(value: String) extends TableFilter[ReportCardEvaluationTable] {
  override def predicate = _.int >= Try(value.toInt).getOrElse(-1)
}

case class MaxIntFilter(value: String) extends TableFilter[ReportCardEvaluationTable] {
  override def predicate = _.int <= Try(value.toInt).getOrElse(-1)
}

trait ReportCardEvaluationDao extends AbstractDao[ReportCardEvaluationTable, ReportCardEvaluationDb, ReportCardEvaluationLike] {

  import scala.concurrent.ExecutionContext.Implicits.global

  override val tableQuery = TableQuery[ReportCardEvaluationTable]

  override protected def toAtomic(query: Query[ReportCardEvaluationTable, ReportCardEvaluationDb, Seq]): Future[Seq[ReportCardEvaluationLike]] = {
    val mandatory = for {
      q <- query
      labwork <- q.labworkFk
      student <- q.studentFk
      course <- labwork.courseFk
      degree <- labwork.degreeFk
      semester <- labwork.semesterFk
      lecturer <- course.lecturerFk
    } yield (q, labwork, student, course, degree, semester, lecturer)

    db.run(mandatory.result.map(_.map {
      case (e, l, u, c, d, s, lec) =>
        val labworkAtom = {
          val courseAtom = CourseAtom(c.label, c.description, c.abbreviation, lec.toUniqueEntity, c.semesterIndex, c.id)
          LabworkAtom(l.label, l.description, s.toUniqueEntity, courseAtom, d.toUniqueEntity, l.subscribable, l.published, l.id)
        }

        ReportCardEvaluationAtom(u.toUniqueEntity, labworkAtom, e.label, e.bool, e.int, e.lastModified.dateTime, e.id)
    }.toSeq))
  }

  override protected def toUniqueEntity(query: Query[ReportCardEvaluationTable, ReportCardEvaluationDb, Seq]): Future[Seq[ReportCardEvaluationLike]] = {
    db.run(query.result.map(_.map(e => ReportCardEvaluation(e.student, e.labwork, e.label, e.bool, e.int, e.lastModified.dateTime, e.id))))
  }

  override protected def existsQuery(entity: ReportCardEvaluationDb): Query[ReportCardEvaluationTable, ReportCardEvaluationDb, Seq] = {
    filterBy(List(StudentFilter(entity.student.toString), LabworkFilter(entity.labwork.toString), LabelFilter(entity.label)))
  }

  override protected def shouldUpdate(existing: ReportCardEvaluationDb, toUpdate: ReportCardEvaluationDb): Boolean = {
    (existing.int != toUpdate.int || existing.bool != toUpdate.bool) &&
      (existing.student == toUpdate.student && existing.labwork == toUpdate.labwork && existing.label == toUpdate.label)
  }
}

final class ReportCardEvaluationDaoImpl @Inject()(val db: PostgresProfile.backend.Database) extends ReportCardEvaluationDao
