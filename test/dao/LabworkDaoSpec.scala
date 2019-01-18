package dao

import models._
import slick.dbio.Effect.Write
import store._

final class LabworkDaoSpec extends AbstractDaoSpec[LabworkTable, LabworkDb, Labwork] with LabworkDao {

  import dao.AbstractDaoSpec._
  import slick.jdbc.PostgresProfile.api._

  override protected def dependencies: DBIOAction[Unit, NoStream, Write] = DBIO.seq(
    TableQuery[SemesterTable].forceInsertAll(semesters),
    TableQuery[DegreeTable].forceInsertAll(degrees),
    TableQuery[UserTable].forceInsertAll(employees),
    TableQuery[CourseTable].forceInsertAll(courses)
  )

  override protected def name: String = "labwork"

  override protected val dbEntity: LabworkDb = LabworkDb("label", "description",semesters.head.id,courses.head.id,degrees.head.id)

  override protected val invalidDuplicateOfDbEntity: LabworkDb = LabworkDb(dbEntity.label, dbEntity.description, dbEntity.semester, dbEntity.course, dbEntity.degree, dbEntity.subscribable, dbEntity.published)

  override protected val invalidUpdateOfDbEntity: LabworkDb = dbEntity.copy(dbEntity.label, dbEntity.description, semesters(1).id, courses(1).id, degrees(1).id, dbEntity.subscribable, dbEntity.published)

  override protected val validUpdateOnDbEntity: LabworkDb = dbEntity.copy("updateLabel", "updateDescription", dbEntity.semester, dbEntity.course, dbEntity.degree)

  override protected val dbEntities: List[LabworkDb] = labworks

  override protected val lwmEntity: Labwork = dbEntity.toLwmModel

  override protected val lwmAtom: PostgresLabworkAtom = {
    val course = courses.find(_.id == dbEntity.course).get
    PostgresLabworkAtom(
      dbEntity.label,
      dbEntity.description,
      semesters.find(_.id == dbEntity.semester).get.toLwmModel,
      PostgresCourseAtom(
        course.label,
        course.description,
        course.abbreviation,
        employees.find(_.id == course.lecturer).get.toLwmModel,
        course.semesterIndex,
        course.id
      ),
      degrees.find(_.id == dbEntity.degree).get.toLwmModel,
      dbEntity.subscribable,
      dbEntity.published,
      dbEntity.id
    )
  }
}
