package services

import java.sql.Date

import models.{PostgresSemester, SemesterDb}
import org.joda.time.LocalDate
import slick.dbio.Effect.Write
import store.SemesterTable

final class SemesterServiceSpec extends AbstractDaoSpec[SemesterTable, SemesterDb, PostgresSemester, PostgresSemester] with SemesterService {
  import slick.driver.PostgresDriver.api._
  import services.AbstractDaoSpec._

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
    SemesterDb(entity.label, "abbrev update", entity.end, entity.start, entity.examStart, lastModified, entity.invalidated, entity.id)
  }

  override protected val validUpdateOnEntity: SemesterDb = {
    SemesterDb(entity.label, "abbrev update", entity.start, entity.end, entity.examStart, lastModified, entity.invalidated, entity.id)
  }

  override protected val entities: List[SemesterDb] = semesters

  override protected val dependencies: DBIOAction[Unit, NoStream, Write] = DBIO.seq()

  override protected val postgresEntity: PostgresSemester = entity.toSemester

  override protected val postgresAtom: PostgresSemester = postgresEntity
}
