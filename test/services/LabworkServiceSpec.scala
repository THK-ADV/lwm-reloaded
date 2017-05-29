package services

import models._
import slick.dbio.Effect.Write
import store._

/**
  * Created by Florian on 16.05.2017.
  */
final class LabworkServiceSpec extends AbstractDaoSpec[LabworkTable, LabworkDb, Labwork] with LabworkService {

  import services.AbstractDaoSpec._
  import slick.driver.PostgresDriver.api._


  override protected def dependencies: DBIOAction[Unit, NoStream, Write] = DBIO.seq(
    semesterService.tableQuery.forceInsertAll(semesters),
    degreeService.tableQuery.forceInsertAll(degrees),
    userService.tableQuery.forceInsertAll(employees),
    courseService.tableQuery.forceInsertAll(courses)
  )

  override protected def name: String = "labwork"

  override protected val dbEntity: LabworkDb = LabworkDb("label", "description",semesters.head.id,courses.head.id,degrees.head.id)

  override protected val invalidDuplicateOfDbEntity: LabworkDb = LabworkDb(dbEntity.label, dbEntity.description, dbEntity.semester, dbEntity.course, dbEntity.degree, dbEntity.subscribable, dbEntity.published)

  override protected val invalidUpdateOfDbEntity: LabworkDb = dbEntity.copy(dbEntity.label, dbEntity.description, semesters(1).id, courses(1).id, degrees(1).id, dbEntity.subscribable, dbEntity.published)

  override protected val validUpdateOnDbEntity: LabworkDb = dbEntity.copy("updateLabel", "updateDescription", dbEntity.semester, dbEntity.course, dbEntity.degree)

  override protected val dbEntities: List[LabworkDb] = labworks

  override protected val lwmEntity: Labwork = dbEntity.toLabwork

  override protected val lwmAtom: PostgresLabworkAtom = {
    val course = courses.find(_.id == dbEntity.course).get
    PostgresLabworkAtom(
      dbEntity.label,
      dbEntity.description,
      semesters.find(_.id == dbEntity.semester).get.toSemester,
      PostgresCourseAtom(
        course.label,
        course.description,
        course.abbreviation,
        employees.find(_.id == course.lecturer).get.toUser,
        course.semesterIndex,
        course.id
      ),
      degrees.find(_.id == dbEntity.degree).get.toDegree,
      dbEntity.subscribable,
      dbEntity.published,
      dbEntity.id
    )
  }

  override protected def courseService: CourseService = {
    lazy val sharedDb = db

    new CourseService {
      override protected def authorityService: AuthorityService = new AuthorityService {
        override protected def roleService: RoleService2 = new RoleService2 {
          override protected def db: _root_.slick.driver.PostgresDriver.backend.DatabaseDef = sharedDb
        }

        override protected def db: _root_.slick.driver.PostgresDriver.backend.DatabaseDef = sharedDb
      }

      override protected def db: _root_.slick.driver.PostgresDriver.backend.DatabaseDef = sharedDb
    }
  }

  override protected def userService: UserService = {
    lazy val sharedDb = db
    new UserService {
      override protected def authorityService: AuthorityService = authorityService

      override protected def degreeService: DegreeService = new DegreeService {
        override protected def db: _root_.slick.driver.PostgresDriver.backend.DatabaseDef = sharedDb
      }

      override protected def labworkApplicationService: LabworkApplicationService2 = new LabworkApplicationService2 {
        override protected def db: _root_.slick.driver.PostgresDriver.backend.DatabaseDef = sharedDb
      }

      override protected def db: _root_.slick.driver.PostgresDriver.backend.DatabaseDef = sharedDb
    }
  }
  override protected def degreeService: DegreeService = {
    lazy val sharedDb = db
    new DegreeService {
      override protected def db: _root_.slick.driver.PostgresDriver.backend.DatabaseDef = sharedDb
    }
  }

  override protected def semesterService: SemesterService = {
    lazy val sharedDb = db
    new SemesterService {
      override protected def db: _root_.slick.driver.PostgresDriver.backend.DatabaseDef = sharedDb
    }
  }

}
