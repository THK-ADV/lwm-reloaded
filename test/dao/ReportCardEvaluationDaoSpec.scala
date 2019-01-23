package dao

import models._
import slick.dbio.DBIO
import slick.lifted.TableQuery
import database._
import slick.jdbc.PostgresProfile.api._

final class ReportCardEvaluationDaoSpec extends AbstractDaoSpec[ReportCardEvaluationTable, ReportCardEvaluationDb, ReportCardEvaluationLike] with ReportCardEvaluationDao {
  import dao.AbstractDaoSpec._

  override protected def name = "reportCardEvaluationSpec"

  override protected val dbEntity: ReportCardEvaluationDb = populateReportCardEvaluations(1, 1)(students, labworks).head

  override protected val invalidDuplicateOfDbEntity: ReportCardEvaluationDb = dbEntity.copy(bool = !dbEntity.bool)

  override protected val invalidUpdateOfDbEntity: ReportCardEvaluationDb = dbEntity.copy(label = "")

  override protected val validUpdateOnDbEntity: ReportCardEvaluationDb = dbEntity.copy(bool = !dbEntity.bool)

  override protected val dbEntities: List[ReportCardEvaluationDb] = reportCardEvaluations

  override protected val lwmEntity: ReportCardEvaluation = dbEntity.toUniqueEntity

  override protected val lwmAtom: ReportCardEvaluationAtom = {
    val labworkAtom = {
      val labwork = labworks.find(_.id == dbEntity.labwork).get
      val semester = semesters.find(_.id == labwork.semester).get
      val course = courses.find(_.id == labwork.course).get
      val lecturer = employees.find(_.id == course.lecturer).get.toUniqueEntity
      val courseAtom = CourseAtom(course.label, course.description, course.abbreviation, lecturer, course.semesterIndex, course.id)
      val degree = degrees.find(_.id == labwork.degree).get

      LabworkAtom(labwork.label, labwork.description, semester.toUniqueEntity, courseAtom, degree.toUniqueEntity, labwork.subscribable, labwork.published, labwork.id)
    }

    ReportCardEvaluationAtom(
      students.find(_.id == lwmEntity.student).get.toUniqueEntity,
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
