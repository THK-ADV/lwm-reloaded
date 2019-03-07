package dao

import database._
import models._
import play.api.inject.guice.GuiceableModule
import slick.dbio.Effect.Write

final class LabworkDaoSpec extends AbstractDaoSpec[LabworkTable, LabworkDb, LabworkLike] {
  import AbstractDaoSpec._
  import slick.jdbc.PostgresProfile.api._

  override protected val dao: LabworkDao = app.injector.instanceOf(classOf[LabworkDao])

  override protected def bindings: Seq[GuiceableModule] = Seq.empty

  override protected def name: String = "labwork"

  override protected val dbEntity: LabworkDb = labworks.head

  override protected val invalidDuplicateOfDbEntity: LabworkDb = LabworkDb(dbEntity.label, dbEntity.description, dbEntity.semester, dbEntity.course, dbEntity.degree, dbEntity.subscribable, dbEntity.published)

  override protected val invalidUpdateOfDbEntity: LabworkDb = dbEntity.copy(dbEntity.label, dbEntity.description, semesters(1).id, courses(1).id, degrees(1).id, dbEntity.subscribable, dbEntity.published)

  override protected val validUpdateOnDbEntity: LabworkDb = dbEntity.copy("updateLabel", "updateDescription", dbEntity.semester, dbEntity.course, dbEntity.degree)

  override protected val dbEntities: List[LabworkDb] = labworks.tail

  override protected val lwmAtom: LabworkAtom = {
    val course = courses.find(_.id == dbEntity.course).get
    LabworkAtom(
      dbEntity.label,
      dbEntity.description,
      semesters.find(_.id == dbEntity.semester).get.toUniqueEntity,
      CourseAtom(
        course.label,
        course.description,
        course.abbreviation,
        employees.find(_.id == course.lecturer).get.toUniqueEntity,
        course.semesterIndex,
        course.id
      ),
      degrees.find(_.id == dbEntity.degree).get.toUniqueEntity,
      dbEntity.subscribable,
      dbEntity.published,
      dbEntity.id
    )
  }

  override protected def dependencies: DBIOAction[Unit, NoStream, Write] = DBIO.seq(
    TableQuery[SemesterTable].forceInsertAll(semesters),
    TableQuery[DegreeTable].forceInsertAll(degrees),
    TableQuery[UserTable].forceInsertAll(employees),
    TableQuery[CourseTable].forceInsertAll(courses)
  )
}
