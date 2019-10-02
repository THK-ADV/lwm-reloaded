package dao

import java.util.UUID

import database.{SemesterDb, SemesterTable}
import models.Semester
import org.joda.time.LocalDate
import play.api.inject.guice.GuiceableModule
import slick.dbio.Effect.Write
import slick.jdbc.PostgresProfile.api._

final class AbstractSemesterDaoSpec extends AbstractDaoSpec[SemesterTable, SemesterDb, Semester] {

  import AbstractDaoSpec._
  import utils.date.DateTimeOps._

  override protected def name: String = "semester"

  override protected val dbEntity: SemesterDb = SemesterDb(
    "label",
    "abbrev",
    LocalDate.parse("2017-01-01").sqlDate,
    LocalDate.parse("2017-01-02").sqlDate,
    LocalDate.parse("2017-01-03").sqlDate
  )

  override protected val invalidDuplicateOfDbEntity: SemesterDb = {
    dbEntity.copy(id = UUID.randomUUID)
  }

  override protected val invalidUpdateOfDbEntity: SemesterDb = {
    dbEntity.copy(label = "updated", start = LocalDate.now.sqlDate)
  }

  override protected val validUpdateOnDbEntity: SemesterDb = {
    dbEntity.copy(abbreviation = "updated", examStart = LocalDate.now.sqlDate)
  }

  override protected val dbEntities: List[SemesterDb] = semesters

  override protected val dependencies: DBIOAction[Unit, NoStream, Write] = DBIO.seq()

  override protected val lwmAtom: Semester = dbEntity.toUniqueEntity

  override protected val dao: SemesterDao = app.injector.instanceOf(classOf[SemesterDao])

  override protected def bindings: Seq[GuiceableModule] = Seq.empty
}