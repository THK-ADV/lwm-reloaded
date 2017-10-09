package dao

import models._
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

  override protected val lwmAtom: PostgresReportCardEvaluationAtom = {
    val labworkAtom = {
      val labwork = labworks.find(_.id == dbEntity.labwork).get
      val semester = semesters.find(_.id == labwork.semester).get
      val course = courses.find(_.id == labwork.course).get
      val lecturer = employees.find(_.id == course.lecturer).get.toLwmModel
      val courseAtom = PostgresCourseAtom(course.label, course.description, course.abbreviation, lecturer, course.semesterIndex, course.id)
      val degree = degrees.find(_.id == labwork.degree).get

      PostgresLabworkAtom(labwork.label, labwork.description, semester.toLwmModel, courseAtom, degree.toLwmModel, labwork.subscribable, labwork.published, labwork.id)
    }

    PostgresReportCardEvaluationAtom(
      students.find(_.id == lwmEntity.student).get.toLwmModel,
      labworkAtom,
      lwmEntity.label,
      lwmEntity.bool,
      lwmEntity.int,
      lwmEntity.lastModified,
      lwmEntity.id
    )
  }

  override protected val dependencies: DBIOAction[Unit, NoStream, Effect.Write] = DBIO.seq(
    TableQuery[DegreeTable].forceInsertAll(degrees),
    TableQuery[UserTable].forceInsertAll(employees ++ students),
    TableQuery[SemesterTable].forceInsertAll(semesters),
    TableQuery[CourseTable].forceInsertAll(courses),
    TableQuery[LabworkTable].forceInsertAll(labworks)
  )
}
