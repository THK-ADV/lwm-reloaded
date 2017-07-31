package dao

import java.util.UUID

import models._
import services._
import slick.dbio.Effect.Write
import slick.driver
import slick.driver.PostgresDriver
import store.{CourseTable, RoleTable, UserTable}

class CourseDaoSpec extends AbstractDaoSpec[CourseTable, CourseDb, Course] with CourseDao {

  import dao.AbstractDaoSpec._
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
  }


  override protected val dbEntity: CourseDb = CourseDb("label", "description", "abbreviation", randomEmployee.id, 3)
  override protected val invalidDuplicateOfDbEntity: CourseDb = CourseDb(dbEntity.label, "description2", "abbreviation2", UUID.randomUUID(), dbEntity.semesterIndex)
  override protected val invalidUpdateOfDbEntity: CourseDb = dbEntity.copy("label2", dbEntity.description, dbEntity.abbreviation, dbEntity.lecturer, 2)
  override protected val validUpdateOnDbEntity: CourseDb = dbEntity.copy(dbEntity.label, "updatedDescription", "updatedAbbreviation", randomEmployee.id, dbEntity.semesterIndex)
  override protected val dbEntities: List[CourseDb] = courses
  override protected val dependencies: DBIOAction[Unit, NoStream, Write] = DBIO.seq(
    TableQuery[UserTable].forceInsertAll(employees)
  )
  override protected val authorityService: AuthorityDao = {
    lazy val sharedDb = db

    new AuthorityDao {
      override protected def roleService: RoleDao = new RoleDao {
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
