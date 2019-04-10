package dao

import database._
import models._
import play.api.inject.guice.GuiceableModule
import slick.dbio.DBIO
import slick.jdbc.PostgresProfile.api._
import slick.lifted.TableQuery

final class ReportCardEvaluationDaoSpec extends AbstractDaoSpec[ReportCardEvaluationTable, ReportCardEvaluationDb, ReportCardEvaluationLike] {

  import AbstractDaoSpec._
  import utils.date.DateTimeOps.SqlTimestampConverter
  import scala.util.Random.{nextBoolean, nextInt}

  override protected def name = "reportCardEvaluation"

  override protected val dbEntity: ReportCardEvaluationDb = ReportCardEvaluationDb(
    randomStudent.id, randomLabwork.id, "label", bool = true, 10
  )

  override protected val invalidDuplicateOfDbEntity: ReportCardEvaluationDb = dbEntity.copy(bool = !dbEntity.bool)

  override protected val invalidUpdateOfDbEntity: ReportCardEvaluationDb = dbEntity.copy(label = "")

  override protected val validUpdateOnDbEntity: ReportCardEvaluationDb = dbEntity.copy(bool = !dbEntity.bool)

  override protected val dbEntities: List[ReportCardEvaluationDb] = labworks take 4 flatMap { l =>
    students take 10 flatMap { s =>
      0 until 5 map (i => ReportCardEvaluationDb(s.id, l.id, i.toString, nextBoolean, nextInt(10)))
    }
  }

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
      students.find(_.id == dbEntity.student).get.toUniqueEntity,
      labworkAtom,
      dbEntity.label,
      dbEntity.bool,
      dbEntity.int,
      dbEntity.lastModified.dateTime,
      dbEntity.id
    )
  }

  override protected val dependencies: DBIOAction[Unit, NoStream, Effect.Write] = DBIO.seq(
    TableQuery[DegreeTable].forceInsertAll(degrees),
    TableQuery[UserTable].forceInsertAll(employees ++ students),
    TableQuery[SemesterTable].forceInsertAll(semesters),
    TableQuery[CourseTable].forceInsertAll(courses),
    TableQuery[LabworkTable].forceInsertAll(labworks)
  )

  override protected val dao: ReportCardEvaluationDao = app.injector.instanceOf(classOf[ReportCardEvaluationDao])

  override protected def bindings: Seq[GuiceableModule] = Seq.empty
}
