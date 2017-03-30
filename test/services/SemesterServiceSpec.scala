package services

import java.sql.Date

import models.{PostgresSemester, SemesterDb}
import org.joda.time.LocalDate
import slick.dbio.Effect.Write
import store.SemesterTable

final class SemesterServiceSpec extends AbstractDaoSpec[SemesterTable, SemesterDb, PostgresSemester] with SemesterService {
  import slick.driver.PostgresDriver.api._

  val maxSemester = 10
  val now = Date.valueOf("2017-01-01")
  val tomorrow = Date.valueOf("2017-01-02")
  val exam = Date.valueOf("2017-01-03")

  "A SemesterServiceSpec " should {

    "return current semester" in {
      val current = entities.map(_.toSemester).filter(PostgresSemester.isCurrent)

      val result = await(get(List(SemesterCurrentFilter(LocalDate.now.toString))))

      result.size shouldBe 1
      result shouldBe current
    }
  }

  override protected def name: String = "semester"

  override protected val entity: SemesterDb = SemesterDb("label", "abbrev", now, tomorrow, exam)

  override protected val invalidDuplicateOfEntity: SemesterDb = {
    SemesterDb(entity.label, "other abbrev", entity.start, entity.end, entity.examStart)
  }

  override protected val invalidUpdateOfEntity: SemesterDb = {
    SemesterDb(entity.label, "abbrev update", entity.end, entity.start, entity.examStart, entity.invalidated, entity.id)
  }

  override protected val validUpdateOnEntity: SemesterDb = {
    SemesterDb(entity.label, "abbrev update", entity.start, entity.end, entity.examStart, entity.invalidated, entity.id)
  }

  override protected val entities: List[SemesterDb] = {
    val template = LocalDate.now.withDayOfWeek(1).withMonthOfYear(9).minusYears(5).plusMonths(6)

    (0 until maxSemester).foldLeft((List.empty[SemesterDb], template)) {
      case ((list, t), i) =>
        val start = new Date(t.plusDays(1).toDateTimeAtStartOfDay.getMillis)
        val end = t.plusDays(1).plusMonths(6)
        val exam = new Date(t.plusDays(1).plusMonths(5).toDateTimeAtStartOfDay.getMillis)

        val current = SemesterDb(i.toString, i.toString, start, new Date(end.toDateTimeAtStartOfDay.getMillis), exam)
        (list.:+(current), end)
    }._1
  }

  override protected def dependencies: DBIOAction[Unit, NoStream, Write] = DBIO.seq()
}
