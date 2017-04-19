package services

import java.sql.Date

import models.{PostgresSemester, SemesterDb}
import org.joda.time.LocalDate
import slick.dbio.Effect.Write
import store.SemesterTable

final class SemesterServiceSpec extends AbstractDaoSpec[SemesterTable, SemesterDb, PostgresSemester] with SemesterService {
  import slick.driver.PostgresDriver.api._
  import services.AbstractDaoSpec._

  val now = Date.valueOf("2017-01-01")
  val tomorrow = Date.valueOf("2017-01-02")
  val exam = Date.valueOf("2017-01-03")

  "A SemesterServiceSpec " should {

    "return current semester" in {
      val current = dbEntities.map(_.toSemester).filter(PostgresSemester.isCurrent)

      val result = await(get(List(SemesterCurrentFilter(LocalDate.now.toString))))

      result.size shouldBe 1
      result shouldBe current
    }
  }

  override protected def name: String = "semester"

  override protected val dbEntity: SemesterDb = SemesterDb("label", "abbrev", now, tomorrow, exam)

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

  override protected val lwmEntity: PostgresSemester = dbEntity.toSemester

  override protected val lwmAtom: PostgresSemester = lwmEntity
}
