package services

import java.util.UUID

import models._
import slick.dbio.Effect.Write
import store.{CourseTable, RoleTable, UserTable}

/**
  * Created by florian on 4/10/17.
  */
class CourseServiceSpec extends AbstractDaoSpec[CourseTable, CourseDb, Course] with CourseService{
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

      await(get(labelFilter, atomic = false)) shouldBe entities.filter(_.label == labelFilterValue).map(_.toCourse)
      await(get(semesterIndexFilter, atomic = false)) shouldBe entities.filter(_.semesterIndex == semesterIndexFilterValue).map(_.toCourse)
      await(get(abbreviationFilter, atomic = false)) shouldBe entities.filter(_.abbreviation == abbreviationFilterValue).map(_.toCourse)
      await(get(abbreviationAndSemesterIndexFilter, atomic = false)) shouldBe entities.filter(course =>
        course.abbreviation == abbreviationAndSemesterIndexFilterAbbreviationValue
          && course.semesterIndex == abbreviationAndSemesterIndexFilterSemesterIndexValue
      ).map(_.toCourse)
      await(get(labelAndSemesterIndexFilter, atomic = false)) shouldBe entities.filter(course =>
        course.label == labelAndSemesterIndexFilterLabelValue
          && course.semesterIndex == labelAndSemesterIndexFilterSemesterIndexValue
      ).map(_.toCourse)
    }

    "create a course with dedicated roles" in {

      val lecturer = randomEmployee

      val result = await(create(CourseDb("TestLabel","TestDescription","TL",lecturer.id,3)))
      val authResult = await(db.run(authorityService.tableQuery.filter(_.user === lecturer.id).result))
      val rolesResult = await(db.run(TableQuery[RoleTable].filter(role => role.label === Roles.CourseManagerLabel || role.label === Roles.RightsManagerLabel).result))
      val rightsManager = rolesResult.find(_.label == Roles.RightsManagerLabel).get
      val courseManager = rolesResult.find(_.label == Roles.CourseManagerLabel).get

      authResult.exists(a => a.user == result.lecturer && a.role == rightsManager.id && a.course.isEmpty) shouldBe true
      authResult.exists(a => a.user == result.lecturer && a.role == courseManager.id && a.course.contains(result.id)) shouldBe true
    }
  }

  override protected def name: String = "course"

  override protected val entity: CourseDb = CourseDb("label", "description", "abbreviation", randomEmployee.id, 3)

  override protected val invalidDuplicateOfEntity: CourseDb = CourseDb(entity.label, "description2", "abbreviation2", UUID.randomUUID(), entity.semesterIndex)

  override protected val invalidUpdateOfEntity: CourseDb = entity.copy("label2", entity.description, entity.abbreviation, entity.lecturer, 2)

  override protected val validUpdateOnEntity: CourseDb = entity.copy(entity.label, "updatedDescription", "updatedAbbreviation", randomEmployee.id, entity.semesterIndex)

  override protected val entities: List[CourseDb] = courses

  override protected val dependencies: DBIOAction[Unit, NoStream, Write] = DBIO.seq(
    TableQuery[UserTable].forceInsertAll(employees)
  )

  override protected def authorityService: AuthorityService = new AuthorityServiceSpec()
}
