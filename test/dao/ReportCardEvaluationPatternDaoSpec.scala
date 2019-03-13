package dao

import java.util.UUID

import database._
import models.ReportCardEvaluationPattern
import play.api.inject.guice.GuiceableModule
import slick.dbio.DBIO
import slick.jdbc.PostgresProfile.api._
import slick.lifted.TableQuery

final class ReportCardEvaluationPatternDaoSpec extends AbstractDaoSpec[ReportCardEvaluationPatternTable, ReportCardEvaluationPatternDb, ReportCardEvaluationPattern] {

  import AbstractDaoSpec._

  override protected def name: String = "reportCardEvaluationPattern"

  override protected val dbEntity: ReportCardEvaluationPatternDb = reportCardEvaluationpatterns.head

  override protected val invalidDuplicateOfDbEntity: ReportCardEvaluationPatternDb = dbEntity.copy(id = UUID.randomUUID)

  override protected val invalidUpdateOfDbEntity: ReportCardEvaluationPatternDb = dbEntity.copy(entryType = "entryType")

  override protected val validUpdateOnDbEntity: ReportCardEvaluationPatternDb = dbEntity.copy(min = 100)

  override protected val dbEntities: List[ReportCardEvaluationPatternDb] = reportCardEvaluationpatterns.tail

  override protected val lwmAtom: ReportCardEvaluationPattern = dbEntity.toUniqueEntity

  override protected val dependencies = DBIO.seq(
    TableQuery[DegreeTable].forceInsertAll(degrees),
    TableQuery[UserTable].forceInsertAll(employees),
    TableQuery[SemesterTable].forceInsertAll(semesters),
    TableQuery[CourseTable].forceInsertAll(courses),
    TableQuery[LabworkTable].forceInsertAll(labworks)
  )

  override protected val dao: ReportCardEvaluationPatternDao = app.injector.instanceOf(classOf[ReportCardEvaluationPatternDao])

  override protected def bindings: Seq[GuiceableModule] = Seq.empty
}
