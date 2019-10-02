package dao

import java.util.UUID

import database._
import models._
import play.api.inject.guice.GuiceableModule
import slick.dbio.Effect.Write

final class LabworkDaoSpec extends AbstractDaoSpec[LabworkTable, LabworkDb, LabworkLike] { // TODO create many fails from time to time

  import AbstractDaoSpec._
  import slick.jdbc.PostgresProfile.api._

  override protected val dao: LabworkDao = app.injector.instanceOf(classOf[LabworkDao])

  override protected def bindings: Seq[GuiceableModule] = Seq.empty

  override protected def name: String = "labwork"

  override protected val dbEntity: LabworkDb =
    LabworkDb("label", "desc", semesters.head.id, courses.head.id, degrees.head.id)

  override protected val invalidDuplicateOfDbEntity: LabworkDb =
    dbEntity.copy(id = UUID.randomUUID)

  override protected val invalidUpdateOfDbEntity: LabworkDb =
    dbEntity.copy(label = "new label", course = UUID.randomUUID, degree = UUID.randomUUID)

  override protected val validUpdateOnDbEntity: LabworkDb =
    dbEntity.copy(description = "updateDescription", published = !dbEntity.published)

  override protected val dbEntities: List[LabworkDb] = (semesters.tail, courses.tail, degrees.tail).zipped.toList.map {
    case (s, c, d) =>
      val r = UUID.randomUUID.toString
      LabworkDb(s"label $r", s"desc $r", s.id, c.id, d.id)
  }

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
