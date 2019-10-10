package dao

import base.{DatabaseSpec, DateGenerator}
import dao.SemesterDao.{endFilter, sinceFilter, startFilter, untilFilter}
import database.SemesterDb
import org.joda.time.LocalDate
import play.api.inject.guice.GuiceableModule
import slick.jdbc.JdbcProfile
import slick.jdbc.PostgresProfile.api._
import utils.date.DateTimeOps._

class SemesterDaoSpec extends DatabaseSpec with DateGenerator {

  override implicit val profile: JdbcProfile = _root_.slick.jdbc.PostgresProfile

  val dao = app.injector.instanceOf(classOf[SemesterDao])

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    runAsync(dao.tableQuery.schema.create)(_ => Unit)
  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    runAsync(dao.tableQuery.schema.drop)(_ => Unit)
  }

  override protected def beforeEach(): Unit = {
    super.beforeEach()
    runAsync(dao.tableQuery.delete)(_ => Unit)
  }

  private def createSemester(i: String, sy: Int, sm: Int, sd: Int, ey: Int, em: Int, ed: Int, exy: Int, exm: Int, exd: Int) = SemesterDb(
    i, i,
    localDate(sy, sm, sd).sqlDate,
    localDate(ey, em, ed).sqlDate,
    localDate(exy, exm, exd).sqlDate
  )

  "A SemesterDaoSpec" should {
    "return current semester" in {
      val now = LocalDate.now

      val semesters = (now.getYear - 5 until now.getYear + 5).map { i =>
        createSemester(i.toString, i, 1, 1, i, 11, 30, i, 12, 1)
      }.toList

      async(dao.createMany(semesters))(_ => Unit)
      async(dao.current(false))(_.value shouldEqual semesters.find(_.start.localDate.getYear == now.getYear).get.toUniqueEntity)
    }

    "not return current semester if there is none" in {
      val now = LocalDate.now

      val semesters = (now.getYear - 20 until now.getYear - 10).map { i =>
        createSemester(i.toString, i, 1, 1, i, 11, 30, i, 12, 1)
      }.toList

      async(dao.createMany(semesters))(_ => Unit)
      async(dao.current(false))(_ shouldBe empty)
    }

    "filter semesters by start and end date " in {
      val semesters = List(
        createSemester("1", 2019, 3, 3, 2019, 5, 3, 2019, 6, 3),
        createSemester("2", 2020, 3, 3, 2020, 5, 3, 2020, 6, 3),
        createSemester("3", 2023, 3, 3, 2023, 5, 3, 2023, 6, 3)
      )

      val start = localDate(2019, 3, 3)
      val end = localDate(2020, 5, 3)

      async(dao.createMany(semesters))(_ => Unit)

      async(dao.get(List(startFilter(start)))) { semester =>
        semester.size shouldBe 1
        semester.head.start shouldBe start
      }

      async(dao.get(List(endFilter(end)))) { semester =>
        semester.size shouldBe 1
        semester.head.end shouldBe end
      }

      async(dao.get(List(startFilter(localDate(2019, 5, 9)))))(_ shouldBe empty)
      async(dao.get(List(endFilter(localDate(2017, 1, 30)))))(_ shouldBe empty)
    }

    "filter semester since and until start date" in {
      val now = LocalDate.now

      val semesters = (now.getYear - 10 until now.getYear + 10).map { i =>
        createSemester(i.toString, i, 1, 1, i, 11, 30, i, 12, 1)
      }.toList

      async(dao.createMany(semesters))(_ => Unit)

      val since1 = now.minusYears(3)
      async(dao.get(List(sinceFilter(since1)))) { semestersSince =>
        semestersSince should contain theSameElementsAs semesters.filter(s => s.start.localDate == since1 || s.start.localDate.isAfter(since1)).map(_.toUniqueEntity)
      }

      val since2 = now.minusYears(11)
      async(dao.get(List(sinceFilter(since2)))) { semestersSince =>
        semestersSince should contain theSameElementsAs semesters.map(_.toUniqueEntity)
      }

      val since3 = now.plusYears(11)
      async(dao.get(List(sinceFilter(since3)))) { semestersSince =>
        semestersSince shouldBe empty
      }

      val until1 = now.minusYears(3)
      async(dao.get(List(untilFilter(until1)))) { semesterUntil =>
        semesterUntil should contain theSameElementsAs semesters.filter(s => s.end.localDate == since1 || s.end.localDate.isBefore(until1)).map(_.toUniqueEntity)
      }

      val until2 = now.plusYears(11)
      async(dao.get(List(untilFilter(until2)))) { semesterUntil =>
        semesterUntil should contain theSameElementsAs semesters.map(_.toUniqueEntity)
      }

      val until3 = now.minusYears(11)
      async(dao.get(List(untilFilter(until3)))) { semesterUntil =>
        semesterUntil shouldBe empty
      }
    }
  }

  override protected def bindings: Seq[GuiceableModule] = Seq.empty
}
