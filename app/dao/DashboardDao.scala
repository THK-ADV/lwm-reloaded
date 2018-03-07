package dao

import java.util.UUID

import slick.driver.PostgresDriver
import slick.driver.PostgresDriver.api._

trait DashboardDao {

  import scala.concurrent.ExecutionContext.Implicits.global

  protected def db: PostgresDriver.backend.Database

  protected def labworkDao: LabworkDao

  protected def labworkApplicationDao: LabworkApplicationDao

  protected def reportCardEntryDao: ReportCardEntryDao

  protected def reportCardEvaluationDao: ReportCardEvaluationDao

  protected def reportCardEvaluationPatternDao: ReportCardEvaluationPatternDao

  protected def groupDao: GroupDao

  def student(student: UUID, degree: UUID, semester: UUID, atomic: Boolean) = {
    for {
      labworks <- labworkDao.filter(l => l.semester === semester && l.degree === degree, atomic)
      labworIds = labworks.map(_.id)
      apps <- labworkApplicationDao.filter(app => app.applicant === student && app.labwork.inSet(labworIds), atomic)
      groups <- groupDao.filter(g => g.contains(student) && g.labwork.inSet(labworIds), atomic)
      cards <- reportCardEntryDao.filter(e => e.student === student && e.labwork.inSet(labworIds), atomic)
      evals <- reportCardEvaluationDao.filter(_.student === student, atomic)
      evalPatterns <- reportCardEvaluationPatternDao.filter(_.labwork.inSet(labworIds), atomic)
    } yield (labworks, apps, groups, cards, evals, evalPatterns)
  }

  def employee(employee: UUID, atomic: Boolean) = ???
}

final class DashboardDaoImpl(val db: PostgresDriver.backend.Database,
                             val labworkDao: LabworkDao,
                             val labworkApplicationDao: LabworkApplicationDao,
                             val reportCardEntryDao: ReportCardEntryDao,
                             val reportCardEvaluationDao: ReportCardEvaluationDao,
                             val reportCardEvaluationPatternDao: ReportCardEvaluationPatternDao,
                             val groupDao: GroupDao) extends DashboardDao