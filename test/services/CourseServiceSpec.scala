package services

import java.util.UUID

import models._
import slick.dbio.Effect.Write
import slick.driver.PostgresDriver
import slick.{driver, lifted}
import store.{CourseTable, RoleTable, UserTable}

/**
  * Created by florian on 4/10/17.
  */
class CourseServiceSpec extends AbstractDaoSpec[CourseTable, CourseDb, Course] with CourseService {

  import services.AbstractDaoSpec._
  import slick.driver.PostgresDriver.api._

  "A CourseServiceSpec " should {

    "filter courses by" in {

      val labelFilterValue = "3"
      val semesterIndexFilterValue = 2
      val abbreviationFilterValue = "4"
      val abbreviationAndSemesterIndexFilterAbbreviationValue = "5"
      val abbreviationAndSemesterIndexFilterSemesterIndexValue = 5
      val labelAndSemesterIndexFilterLabelValue = "6"
      val labelAndSemesterIndexFilterSemesterIndexValue = 6

      val labelFilter = List(
        CourseLabelFilter(labelFilterValue)
      )
      val semesterIndexFilter = List(
        CourseSemesterIndexFilter(semesterIndexFilterValue.toString)
      )
      val abbreviationFilter = List(
        CourseAbbreviationFilter(abbreviationFilterValue)
      )
      val abbreviationAndSemesterIndexFilter = List(
        CourseAbbreviationFilter(abbreviationAndSemesterIndexFilterAbbreviationValue),
        CourseSemesterIndexFilter(abbreviationAndSemesterIndexFilterSemesterIndexValue.toString)
      )
      val labelAndSemesterIndexFilter = List(
        CourseLabelFilter(labelAndSemesterIndexFilterLabelValue),
        CourseSemesterIndexFilter(labelAndSemesterIndexFilterSemesterIndexValue.toString)
      )

      await(get(labelFilter, atomic = false)) shouldBe dbEntities.filter(_.label == labelFilterValue).map(_.toLwmModel)
      await(get(semesterIndexFilter, atomic = false)) shouldBe dbEntities.filter(_.semesterIndex == semesterIndexFilterValue).map(_.toLwmModel)
      await(get(abbreviationFilter, atomic = false)) shouldBe dbEntities.filter(_.abbreviation == abbreviationFilterValue).map(_.toLwmModel)
      await(get(abbreviationAndSemesterIndexFilter, atomic = false)) shouldBe dbEntities.filter(course =>
        course.abbreviation == abbreviationAndSemesterIndexFilterAbbreviationValue
          && course.semesterIndex == abbreviationAndSemesterIndexFilterSemesterIndexValue
      ).map(_.toLwmModel)
      await(get(labelAndSemesterIndexFilter, atomic = false)) shouldBe dbEntities.filter(course =>
        course.label == labelAndSemesterIndexFilterLabelValue
          && course.semesterIndex == labelAndSemesterIndexFilterSemesterIndexValue
      ).map(_.toLwmModel)
    }

    //"create a course with dedicated roles" in {
//
    //  val lecturer = randomEmployee
//
    //  await(db.run(
    //    for {
    //      result <- createManyExpandedQuery(Seq(CourseDb("TestLabel", "TestDescription", "TL", lecturer.id, 3))).map(_.head)
    //      auths <- authorityService.tableQuery.filter(_.user === lecturer.id).result
    //      rightsManager <- getRightsManager
    //      courseManager <- getCourseManager
    //    } yield {
    //      auths.exists(a => a.user == result.lecturer && a.role == rightsManager.get.id && a.course.isEmpty) shouldBe true
    //      auths.exists(a => a.user == result.lecturer && a.role == courseManager.get.id && a.course.contains(result.id)) shouldBe true
    //    }
    //  ))
    //}
//
    //"update a course with dedicated roles" in {
    //  val oldLecturer = employees(0)
    //  val newLecturer = employees(1)
//
    //  val course = CourseDb("TestLabel2", "TestDescription2", "TL2", oldLecturer.id, 3)
//
    //  await(db.run(
    //    for {
    //      oldCourse <- createManyExpandedQuery(Seq(course)).map(_.head)
    //      _ <- updateExpandedQuery(oldCourse.copy(oldCourse.label, oldCourse.description, oldCourse.abbreviation, newLecturer.id, oldCourse.semesterIndex))
//
    //      authOldLecturer <- authorityService.tableQuery.filter(_.user === oldLecturer.id).result
    //      authNewLecturer <- authorityService.tableQuery.filter(_.user === newLecturer.id).result
//
    //      courseManager <- getCourseManager
    //    } yield {
    //      authOldLecturer.exists(a => a.role == courseManager.get.id && a.course.contains(oldCourse.id)) shouldBe false
    //      authNewLecturer.exists(a => a.role == courseManager.get.id && a.course.contains(oldCourse.id)) shouldBe true
    //    }
    //  ))
    //}
//
    //"delete a course with dedicated roles" in {
    //  val course = randomCourse
//
    //  await(db.run(
    //    for {
    //      _ <- deleteExpandedQuery(course)
    //      auths <- authorityService.tableQuery.filter(_.user === course.lecturer).result
    //      rightsManager <- getRightsManager
    //      courseManager <- getCourseManager
    //    } yield {
    //      auths.exists(a => a.user == course.lecturer && a.role == courseManager.get.id && a.course.contains(course.id)) shouldBe false
    //      val hasOtherCourses = auths.exists(a => a.user == course.lecturer && a.role == courseManager.get.id)
    //      auths.exists(a => a.role == rightsManager.get.id) shouldBe hasOtherCourses
    //    }))
//
    //}
//
    "delete RightsManager only if no course left" in {
      val lecturer = DbUser("ai123", "lastname", "firstname", "email@email.email", User.EmployeeType, None, None)
      val course1 = new CourseDb("label1", "desc1", "abb1", lecturer.id, 1)
      val course2 = new CourseDb("label2", "desc2", "abb2", lecturer.id, 2)


      await(db.run(DBIO.seq(
        TableQuery[UserTable] += lecturer
      ).andThen(
        createManyExpandedQuery(Seq(course1, course2))
      ).andThen(
        shoulhHaveRightsManager(lecturer.id, flag = true)
      ).andThen(
        deleteExpandedQuery(course1)
      ).andThen(
        shoulhHaveRightsManager(lecturer.id, flag = true)
      ).andThen(
        deleteExpandedQuery(course2)
      ).andThen(
        shoulhHaveRightsManager(lecturer.id, flag = false)
      )))
    }
  }


  override protected val dbEntity: CourseDb = CourseDb("label", "description", "abbreviation", randomEmployee.id, 3)
  override protected val invalidDuplicateOfDbEntity: CourseDb = CourseDb(dbEntity.label, "description2", "abbreviation2", UUID.randomUUID(), dbEntity.semesterIndex)
  override protected val invalidUpdateOfDbEntity: CourseDb = dbEntity.copy("label2", dbEntity.description, dbEntity.abbreviation, dbEntity.lecturer, 2)
  override protected val validUpdateOnDbEntity: CourseDb = dbEntity.copy(dbEntity.label, "updatedDescription", "updatedAbbreviation", randomEmployee.id, dbEntity.semesterIndex)
  override protected val dbEntities: List[CourseDb] = courses
  override protected val dependencies: DBIOAction[Unit, NoStream, Write] = DBIO.seq(
    TableQuery[UserTable].forceInsertAll(employees)
  )
  override protected val authorityService: AuthorityService = {
    lazy val sharedDb = db

    new AuthorityService {
      override protected def roleService: RoleService2 = new RoleService2 {
        override protected def db: driver.PostgresDriver.backend.Database = sharedDb
      }

      override protected def db: PostgresDriver.backend.Database = sharedDb
    }
  }

  override protected val lwmEntity: Course = dbEntity.toLwmModel

  override protected val lwmAtom: Course = {
    PostgresCourseAtom(
      dbEntity.label,
      dbEntity.description,
      dbEntity.abbreviation,
      employees.find(_.id == dbEntity.lecturer).get.toLwmModel,
      dbEntity.semesterIndex,
      dbEntity.id
    )
  }

  override protected def name: String = "course"

  private def deleteExpandedQuery(course: CourseDb) = {
    deleteQuery(course.id).flatMap { _ =>
      databaseExpander.get.expandDeleteOf(course)
    }
  }

  private def createManyExpandedQuery(courses: Seq[CourseDb]) = {
    createManyQuery(courses).flatMap {
      _ => databaseExpander.get.expandCreationOf(courses)
    }
  }

  private def updateExpandedQuery(course: CourseDb) = {
    updateQuery(course).flatMap {
      _ => databaseExpander.get.expandUpdateOf(course)
    }
  }

  private def shoulhHaveRightsManager(lecturer: UUID, flag: Boolean) = {
    getRightsManager.flatMap { rm =>
      authorityService.tableQuery.filter(auth => auth.user === lecturer && auth.role === rm.head.id).result.map { auths =>
        if (flag) auths.size shouldBe 1 else auths shouldBe empty
        auths.exists(a => a.user == lecturer && a.role == rm.head.id) shouldBe flag
      }
    }
  }

  private def getRightsManager = {
    TableQuery[RoleTable].filter(role => role.label === Roles.RightsManagerLabel).result.headOption
  }

  private def getCourseManager = {
    TableQuery[RoleTable].filter(role => role.label === Roles.CourseManagerLabel).result.headOption
  }

}
