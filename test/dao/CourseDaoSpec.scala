package dao

import java.util.UUID

import database.{CourseDb, CourseTable, UserTable}
import models._
import play.api.inject.guice.GuiceableModule
import slick.dbio.Effect.Write

class CourseDaoSpec extends AbstractDaoSpec[CourseTable, CourseDb, CourseLike] {

  import AbstractDaoSpec._
  import slick.jdbc.PostgresProfile.api._

  override protected val dao: CourseDao = app.injector.instanceOf(classOf[CourseDao])

  "A CourseServiceSpec " should {

    "filter courses by label" in {
      val labelFilter = List(CourseLabelFilter("3"))
      async(dao.get(labelFilter, atomic = false))(_ shouldBe dbEntities.filter(_.label == "3").map(_.toUniqueEntity))
    }

    "filter courses by semester index" in {
      val semesterIndexFilter = List(CourseSemesterIndexFilter("2"))
      async(dao.get(semesterIndexFilter, atomic = false))(_ shouldBe dbEntities.filter(_.semesterIndex == 2).map(_.toUniqueEntity))
    }

    "filter courses by abbreviation" in {
      val abbreviationFilter = List(CourseAbbreviationFilter("4"))
      async(dao.get(abbreviationFilter, atomic = false))(_ shouldBe dbEntities.filter(_.abbreviation == "4").map(_.toUniqueEntity))
    }

    "filter courses by abbreviation and semester index" in {
      val abbreviationAndSemesterIndexFilter = List(
        CourseAbbreviationFilter("5"),
        CourseSemesterIndexFilter("2")
      )

      async(dao.get(abbreviationAndSemesterIndexFilter, atomic = false))(
        _ shouldBe dbEntities
          .filter(c => c.abbreviation == "5" && c.semesterIndex == 2)
          .map(_.toUniqueEntity)
      )
    }

    "filter courses by label and semester" in {
      val labelAndSemesterIndexFilter = List(
        CourseLabelFilter("six"),
        CourseSemesterIndexFilter("6")
      )

      async(dao.get(labelAndSemesterIndexFilter, atomic = false))(
        _ shouldBe dbEntities
          .filter(course => course.label == "six" && course.semesterIndex == 6)
          .map(_.toUniqueEntity)
      )
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

  override protected val lwmAtom: CourseLike = CourseAtom(
    dbEntity.label,
    dbEntity.description,
    dbEntity.abbreviation,
    employees.find(_.id == dbEntity.lecturer).get.toUniqueEntity,
    dbEntity.semesterIndex, dbEntity.id
  )

  override protected def name: String = "course"

  override protected def bindings: Seq[GuiceableModule] = Seq.empty
}
