package dao

import database.{SemesterDb, SemesterTable}
import models.Semester
import org.joda.time.LocalDate
import play.api.inject.guice.GuiceableModule
import slick.dbio.Effect.Write
import slick.jdbc.PostgresProfile.api._

final class SemesterDaoSpec extends AbstractDaoSpec[SemesterTable, SemesterDb, Semester] {

  import AbstractDaoSpec._
  import utils.LwmDateTime._

  val now = LocalDate.parse("2017-01-01")
  val tomorrow = LocalDate.parse("2017-01-02")
  val exam = LocalDate.parse("2017-01-03")

  "A SemesterServiceSpec" should {

    "TODO" in {} // TODO

//    "return current semester" in {
//      val current = dbEntities.map(_.toUniqueEntity).filter(Semester.isCurrent).head
//
//      async(dao.current(false)) { result =>
//        result.value shouldBe current
//      }
//    }
//
//    "filter semesters by start date" in {
//      val start = randomSemester.start
//      runAsync(dao.filterBy(List(SemesterStartFilter(start.stringMillis))).result) { semester =>
//        semester.size shouldBe 1
//        semester.head.start shouldBe start
//      }
//    }
//
//    "filter semesters by end date" in {
//      val end = randomSemester.end
//      runAsync(dao.filterBy(List(SemesterEndFilter(end.stringMillis))).result) { semester =>
//        semester.size shouldBe 1
//        semester.head.end shouldBe end
//      }
//    }
//
//    "filter semesters since start date" in {
//      val start = randomSemester.start
//      runAsync(dao.filterBy(List(SemesterSinceFilter(start.stringMillis))).result) { semester =>
//        semester.nonEmpty shouldBe true
//        semester.forall(s => s.start.equals(start) || s.start.after(start)) shouldBe true
//      }
//    }
//
//    "filter semesters until end date" in {
//      val end = randomSemester.end
//      runAsync(dao.filterBy(List(SemesterUntilFilter(end.stringMillis))).result) { semester =>
//        semester.nonEmpty shouldBe true
//        semester.forall(s => s.end.equals(end) || s.end.before(end)) shouldBe true
//      }
//    }
  }

  override protected def name: String = "semester"

  override protected val dbEntity: SemesterDb = SemesterDb("label", "abbrev", now.sqlDate, tomorrow.sqlDate, exam.sqlDate)

  override protected val invalidDuplicateOfDbEntity: SemesterDb = {
    SemesterDb(dbEntity.label, "other abbrev", dbEntity.start, dbEntity.end, dbEntity.examStart)
  }

  override protected val invalidUpdateOfDbEntity: SemesterDb = {
    SemesterDb(dbEntity.label, "abbrev update", dbEntity.end, dbEntity.start, dbEntity.examStart, lastModified, dbEntity.invalidated, dbEntity.id)
  }

  override protected val validUpdateOnDbEntity: SemesterDb = {
    SemesterDb(dbEntity.label, "abbrev update", dbEntity.start, dbEntity.end, dbEntity.examStart, lastModified, dbEntity.invalidated, dbEntity.id)
  }

  override protected val dbEntities: List[SemesterDb] = semesters

  override protected val dependencies: DBIOAction[Unit, NoStream, Write] = DBIO.seq()

  override protected val lwmAtom: Semester = dbEntity.toUniqueEntity

  override protected val dao: SemesterDao = app.injector.instanceOf(classOf[SemesterDao])

  override protected def bindings: Seq[GuiceableModule] = Seq.empty
}