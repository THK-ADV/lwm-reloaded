package dao

import models.{PostgresReportCardEntryType, ReportCardEntryTypeDb}
import slick.dbio.{DBIO, Effect}
import slick.lifted.TableQuery
import store._
import slick.driver.PostgresDriver.api._

final class ReportCardEntryTypeDaoSpec extends AbstractDaoSpec[ReportCardEntryTypeTable, ReportCardEntryTypeDb, PostgresReportCardEntryType] with ReportCardEntryTypeDao {
  import dao.AbstractDaoSpec._

  "A ReportCardEntryTypeDaoSpec also" should {
    "update certain fields of entryType properly" in {
      import scala.util.Random.shuffle

      val cards = populateReportCardEntries(1, 8, withRescheduledAndRetry = false)(labworks, students)
      val entryTypes = cards.flatMap(_.entryTypes)

      run(DBIO.seq(
        tableQuery.delete,
        TableQuery[ReportCardEntryTable].delete,
        TableQuery[ReportCardEntryTable].forceInsertAll(cards),
        tableQuery.forceInsertAll(entryTypes)
      ))

      val shuffled = shuffle(entryTypes)
      val first = shuffled(0)
      val second = shuffled(1)
      val third = shuffled(2)
      val fourth = shuffled(3)
      val fifth = shuffled(4)
      val sixth = shuffled(5)

      await(updateFields(first.id, Some(true), 0))
      await(updateFields(second.id, Some(true), 10))
      await(updateFields(third.id, Some(false), 0))
      await(updateFields(fourth.id, Some(false), 5))
      await(updateFields(fifth.id, None, 0))
      await(updateFields(sixth.id, None, 3))

      await(getMany(shuffled.take(6).map(_.id))).foreach { e =>
        e.id match {
          case first.id =>
            e.bool shouldBe Some(true)
            e.int shouldBe 0
          case second.id =>
            e.bool shouldBe Some(true)
            e.int shouldBe 10
          case third.id =>
            e.bool shouldBe Some(false)
            e.int shouldBe 0
          case fourth.id =>
            e.bool shouldBe Some(false)
            e.int shouldBe 5
          case fifth.id =>
            e.bool shouldBe None
            e.int shouldBe 0
          case sixth.id =>
            e.bool shouldBe None
            e.int shouldBe 3
          case _ => fail("no id found")
        }
      }
    }
  }

  private val card = populateReportCardEntries(1, 1, withRescheduledAndRetry = false)(labworks, students).head
  private val cards = populateReportCardEntries(5, 5, withRescheduledAndRetry = false)(labworks, students)

  override protected def name: String = "reportCardEntryType"

  override protected val dbEntity: ReportCardEntryTypeDb = card.entryTypes.head

  override protected val invalidDuplicateOfDbEntity: ReportCardEntryTypeDb = dbEntity

  override protected val invalidUpdateOfDbEntity: ReportCardEntryTypeDb = dbEntity.copy(entryType = "entryType")

  override protected val validUpdateOnDbEntity: ReportCardEntryTypeDb = dbEntity.copy(bool = Some(true), int = 10)

  override protected val dbEntities: List[ReportCardEntryTypeDb] = cards.flatMap(_.entryTypes)

  override protected val lwmEntity: PostgresReportCardEntryType = dbEntity.toLwmModel

  override protected val lwmAtom: PostgresReportCardEntryType = lwmEntity

  override protected val dependencies: DBIOAction[Unit, NoStream, Effect.Write] = DBIO.seq(
    TableQuery[DegreeTable].forceInsertAll(degrees),
    TableQuery[UserTable].forceInsertAll(employees ++ students),
    TableQuery[SemesterTable].forceInsertAll(semesters),
    TableQuery[CourseTable].forceInsertAll(courses),
    TableQuery[LabworkTable].forceInsertAll(labworks),
    TableQuery[RoomTable].forceInsertAll(rooms),
    TableQuery[ReportCardEntryTable].forceInsertAll(cards ++ List(card))
  )
}
