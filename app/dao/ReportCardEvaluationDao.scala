package dao

import java.util.UUID

import models._
import slick.lifted.TableQuery
import store.{PostgresDatabase, ReportCardEvaluationTable, TableFilter}
import utils.LwmDateTime.SqlTimestampConverter
import slick.jdbc.PostgresProfile
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.Future
import scala.util.Try

//case class StudentFilter(value: String) extends TableFilter[ReportCardEvaluationTable] {
//  override def predicate = _.student === UUID.fromString(value)
//}
//
//case class LabworkFilter(value: String) extends TableFilter[ReportCardEvaluationTable] {
//  override def predicate = _.labwork === UUID.fromString(value)
//}
//
//case class CourseFilter(value: String) extends TableFilter[ReportCardEvaluationTable] {
//  override def predicate = _.labworkFk.map(_.course).filter(_ === UUID.fromString(value)).exists
//}
//
//case class LabelFilter(value: String) extends TableFilter[ReportCardEvaluationTable] {
//  override def predicate = _.label === value
//}
//
//case class BoolFilter(value: String) extends TableFilter[ReportCardEvaluationTable] {
//  override def predicate = _.bool === Try(value.toBoolean).getOrElse(false)
//}
//
//case class IntFilter(value: String) extends TableFilter[ReportCardEvaluationTable] {
//  override def predicate = _.int === Try(value.toInt).getOrElse(-1)
//}
//
//case class MinIntFilter(value: String) extends TableFilter[ReportCardEvaluationTable] {
//  override def predicate = _.int >= Try(value.toInt).getOrElse(-1)
//}
//
//case class MaxIntFilter(value: String) extends TableFilter[ReportCardEvaluationTable] {
//  override def predicate = _.int <= Try(value.toInt).getOrElse(-1)
//}
//
//trait ReportCardEvaluationDao extends AbstractDao[ReportCardEvaluationTable, ReportCardEvaluationDb, ReportCardEvaluation] {
//  import scala.concurrent.ExecutionContext.Implicits.global
//
//  override val tableQuery = TableQuery[ReportCardEvaluationTable]
//
//  override protected def toAtomic(query: Query[ReportCardEvaluationTable, ReportCardEvaluationDb, Seq]): Future[Seq[ReportCardEvaluation]] = collectDependencies(query) {
//    case ((e, (l, c, d, s, lec), u)) =>
//      val labworkAtom = {
//        val courseAtom = PostgresCourseAtom(c.label, c.description, c.abbreviation, lec.toLwmModel, c.semesterIndex, c.id)
//        PostgresLabworkAtom(l.label, l.description, s.toLwmModel, courseAtom, d.toLwmModel, l.subscribable, l.published, l.id)
//      }
//
//      PostgresReportCardEvaluationAtom(u.toLwmModel, labworkAtom, e.label, e.bool, e.int, e.lastModified.dateTime, e.id)
//  }
//
//  override protected def toUniqueEntity(query: Query[ReportCardEvaluationTable, ReportCardEvaluationDb, Seq]): Future[Seq[ReportCardEvaluation]] = collectDependencies(query) {
//    case ((e, _, _)) => PostgresReportCardEvaluation(e.student, e.labwork, e.label, e.bool, e.int, e.lastModified.dateTime, e.id)
//  }
//
//  private def collectDependencies(query: Query[ReportCardEvaluationTable, ReportCardEvaluationDb, Seq])
//                                 (build: (ReportCardEvaluationDb, (LabworkDb, CourseDb, DegreeDb, SemesterDb, DbUser), DbUser) => ReportCardEvaluation) = {
//    val mandatory = for {
//      q <- query
//      l <- q.labworkFk
//      s <- q.studentFk
//      (cou, deg, sem) <- l.fullJoin
//      lec <- cou.joinLecturer
//    } yield (q, l, s, cou, deg, sem, lec)
//
//    db.run(mandatory.result.map(_.map {
//      case (e, l, u, cou, deg, sem, lec) => build(e, (l, cou, deg, sem, lec), u)
//    }.toSeq))
//  }
//
//  override protected def existsQuery(entity: ReportCardEvaluationDb): Query[ReportCardEvaluationTable, ReportCardEvaluationDb, Seq] = {
//    filterBy(List(StudentFilter(entity.student.toString), LabworkFilter(entity.labwork.toString), LabelFilter(entity.label)))
//  }
//
//  override protected def shouldUpdate(existing: ReportCardEvaluationDb, toUpdate: ReportCardEvaluationDb): Boolean = {
//    (existing.int != toUpdate.int || existing.bool != toUpdate.bool) &&
//      (existing.student == toUpdate.student && existing.labwork == toUpdate.labwork && existing.label == toUpdate.label)
//  }
//}
//
//final class ReportCardEvaluationDaoImpl(val database: PostgresDatabase) extends ReportCardEvaluationDao
