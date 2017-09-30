package dao

import models.{PostgresReportCardEvaluation, PostgresReportCardEvaluationAtom, ReportCardEvaluation, ReportCardEvaluationDb}
import slick.dbio.DBIO
import slick.lifted.TableQuery
import store._
import slick.driver.PostgresDriver.api._

final class ReportCardEvaluationDaoSpec extends AbstractDaoSpec[ReportCardEvaluationTable, ReportCardEvaluationDb, ReportCardEvaluation] with ReportCardEvaluationDao {
  import dao.AbstractDaoSpec._

  override protected def name = "reportCardEvaluationSpec"

  override protected val dbEntity: ReportCardEvaluationDb = populateReportCardEvaluations(1, 1)(students, labworks).head

  override protected val invalidDuplicateOfDbEntity: ReportCardEvaluationDb = dbEntity.copy(bool = !dbEntity.bool)

  override protected val invalidUpdateOfDbEntity: ReportCardEvaluationDb = dbEntity.copy(label = "")

  override protected val validUpdateOnDbEntity: ReportCardEvaluationDb = dbEntity.copy(bool = !dbEntity.bool)

  override protected val dbEntities: List[ReportCardEvaluationDb] = reportCardEvaluations

  override protected val lwmEntity: PostgresReportCardEvaluation = dbEntity.toLwmModel

  override protected val lwmAtom: PostgresReportCardEvaluationAtom = PostgresReportCardEvaluationAtom(
    students.find(_.id == lwmEntity.student).get.toLwmModel,
    labworks.find(_.id == lwmEntity.labwork).get.toLwmModel,
    lwmEntity.label,
    lwmEntity.bool,
    lwmEntity.int,
    lwmEntity.lastModified,
    lwmEntity.id
  )

  override protected val dependencies: DBIOAction[Unit, NoStream, Effect.Write] = DBIO.seq(
    TableQuery[DegreeTable].forceInsertAll(degrees),
    TableQuery[UserTable].forceInsertAll(employees ++ students),
    TableQuery[SemesterTable].forceInsertAll(semesters),
    TableQuery[CourseTable].forceInsertAll(courses),
    TableQuery[LabworkTable].forceInsertAll(labworks)
  )
}
