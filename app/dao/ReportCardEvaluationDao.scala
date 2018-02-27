package dao

import java.util.UUID

import models._
import slick.driver.PostgresDriver
import slick.lifted.TableQuery
import store.{ReportCardEvaluationTable, TableFilter}
import slick.driver.PostgresDriver.api._
import utils.LwmDateTime.SqlTimestampConverter
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

trait ReportCardEvaluationDao extends AbstractDao[ReportCardEvaluationTable, ReportCardEvaluationDb, ReportCardEvaluation] {
  import scala.concurrent.ExecutionContext.Implicits.global

  override val tableQuery = TableQuery[ReportCardEvaluationTable]

  override protected def toAtomic(query: Query[ReportCardEvaluationTable, ReportCardEvaluationDb, Seq]): DBIOAction[Seq[PostgresReportCardEvaluationAtom], NoStream, Effect.Read] = {
    val mandatory = for {
      q <- query
      l <- q.labworkFk
      s <- q.studentFk
      (cou, deg, sem) <- l.fullJoin
      lec <- cou.joinLecturer
    } yield (q, l, s, cou, deg, sem, lec)

    mandatory.result.map(_.map {
      case (e, l, u, c, d, s, lec) =>
        val labworkAtom = {
          val courseAtom = PostgresCourseAtom(c.label, c.description, c.abbreviation, lec.toLwmModel, c.semesterIndex, c.id)
          PostgresLabworkAtom(l.label, l.description, s.toLwmModel, courseAtom, d.toLwmModel, l.subscribable, l.published, l.id)
        }

        PostgresReportCardEvaluationAtom(u.toLwmModel, labworkAtom, e.label, e.bool, e.int, e.lastModified.dateTime, e.id)
    })
  }

  override protected def toUniqueEntity(query: Query[ReportCardEvaluationTable, ReportCardEvaluationDb, Seq]): DBIOAction[Seq[PostgresReportCardEvaluation], NoStream, Effect.Read] = {
    query.result.map(_.map(_.toLwmModel))
  }

  override protected def existsQuery(entity: ReportCardEvaluationDb): Query[ReportCardEvaluationTable, ReportCardEvaluationDb, Seq] = {
    filterBy(List(StudentFilter(entity.student.toString), LabworkFilter(entity.labwork.toString), LabelFilter(entity.label)))
  }

  override protected def shouldUpdate(existing: ReportCardEvaluationDb, toUpdate: ReportCardEvaluationDb): Boolean = {
    (existing.int != toUpdate.int || existing.bool != toUpdate.bool) &&
      (existing.student == toUpdate.student && existing.labwork == toUpdate.labwork && existing.label == toUpdate.label)
  }
}

final class ReportCardEvaluationDaoImpl(val db: PostgresDriver.backend.Database) extends ReportCardEvaluationDao
