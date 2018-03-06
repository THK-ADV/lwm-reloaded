package dao

import java.util.UUID

import models.{ReportCardEvaluationPattern, ReportCardEvaluationPatternDb}
import slick.dbio.DBIO
import slick.lifted.TableQuery
import store._
import slick.driver.PostgresDriver.api._

final class ReportCardEvaluationPatternDaoSpec extends AbstractDaoSpec[ReportCardEvaluationPatternTable, ReportCardEvaluationPatternDb, ReportCardEvaluationPattern] with ReportCardEvaluationPatternDao {
  import dao.AbstractDaoSpec.{reportCardEvaluationpatterns, labworks, semesters, degrees, courses, employees}

  override protected def name: String = "reportCardEvaluationPattern"

  override protected val dbEntity: ReportCardEvaluationPatternDb = reportCardEvaluationpatterns.head

  override protected val invalidDuplicateOfDbEntity: ReportCardEvaluationPatternDb = dbEntity.copy(id = UUID.randomUUID)

  override protected val invalidUpdateOfDbEntity: ReportCardEvaluationPatternDb = dbEntity.copy(entryType = "entryType")

  override protected val validUpdateOnDbEntity: ReportCardEvaluationPatternDb = dbEntity.copy(min = 100)

  override protected val dbEntities: List[ReportCardEvaluationPatternDb] = reportCardEvaluationpatterns.tail

  override protected val lwmEntity: ReportCardEvaluationPattern = dbEntity.toLwmModel

  override protected val lwmAtom: ReportCardEvaluationPattern = lwmEntity

  override protected val dependencies = DBIO.seq(
    TableQuery[DegreeTable].forceInsertAll(degrees),
    TableQuery[UserTable].forceInsertAll(employees),
    TableQuery[SemesterTable].forceInsertAll(semesters),
    TableQuery[CourseTable].forceInsertAll(courses),
    TableQuery[LabworkTable].forceInsertAll(labworks)
  )
}
