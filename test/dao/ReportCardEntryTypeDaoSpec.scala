package dao

import database._
import models.ReportCardEntryType
import play.api.inject.guice.GuiceableModule
import slick.dbio.{DBIO, Effect}
import slick.jdbc.PostgresProfile.api._
import slick.lifted.TableQuery

final class ReportCardEntryTypeDaoSpec extends AbstractDaoSpec[ReportCardEntryTypeTable, ReportCardEntryTypeDb, ReportCardEntryType] {

  import AbstractDaoSpec._

  import scala.util.Random.{nextBoolean, nextInt}

  "A ReportCardEntryTypeDaoSpec also" should {
    "update certain fields of entryType properly" in {
      import scala.util.Random.shuffle

      val cards = populateReportCardEntries(1, 8, withRescheduledAndRetry = false)(labworks, students)
      val entryTypes = cards.flatMap(_.entryTypes)

      val shuffled = shuffle(entryTypes)
      val first = shuffled.head
      val second = shuffled(1)
      val third = shuffled(2)
      val fourth = shuffled(3)
      val fifth = shuffled(4)
      val sixth = shuffled(5)
      val seven = shuffled(6)
      val eight = shuffled(7)

      runAsyncSequence(
        dao.tableQuery.delete,
        TableQuery[ReportCardEntryTable].delete,
        TableQuery[ReportCardEntryTable].forceInsertAll(cards),
        dao.tableQuery.forceInsertAll(entryTypes)
      )

      async(dao.updateFields(first.id, Some(true), 0))(_ == 3)
      async(dao.updateFields(second.id, Some(true), 10))(_ == 3)
      async(dao.updateFields(third.id, Some(false), 0))(_ == 3)
      async(dao.updateFields(fourth.id, Some(false), 5))(_ == 3)
      async(dao.updateFields(fifth.id, None, 0))(_ == 3)
      async(dao.updateFields(sixth.id, None, 3))(_ == 3)
      async(dao.updateFields(seven.id, seven.bool, 5))(_ == 2)
      async(dao.updateFields(eight.id, eight.bool, eight.int))(_ == 0)

      async(dao.getMany(shuffled.take(8).map(_.id)))(_.foreach { e =>
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
          case seven.id =>
            e.bool shouldBe seven.bool
            e.int shouldBe 5
          case eight.id =>
            e.bool shouldBe eight.bool
            e.int shouldBe eight.int
          case _ => fail("no id found")
        }
      })
    }
  }

  private val cards = populateReportCardEntries(5, 5, withRescheduledAndRetry = false)(labworks, students)

  override protected def name: String = "reportCardEntryType"

  override protected val dbEntity: ReportCardEntryTypeDb = ReportCardEntryTypeDb(Some(cards.head.id), None, "entryType", Some(true), 10)

  override protected val invalidDuplicateOfDbEntity: ReportCardEntryTypeDb = dbEntity

  override protected val invalidUpdateOfDbEntity: ReportCardEntryTypeDb = dbEntity.copy(entryType = "entryType")

  override protected val validUpdateOnDbEntity: ReportCardEntryTypeDb = dbEntity.copy(bool = Some(false), int = 3)

  override protected val dbEntities: List[ReportCardEntryTypeDb] = cards.tail flatMap { card =>
    0 until 5 map (i => ReportCardEntryTypeDb(Some(card.id), None, i.toString, if (nextBoolean) Some(nextBoolean) else None, nextInt(10)))
  }

  override protected val lwmAtom: ReportCardEntryType = dbEntity.toUniqueEntity

  override protected val dependencies: DBIOAction[Unit, NoStream, Effect.Write] = DBIO.seq(
    TableQuery[DegreeTable].forceInsertAll(degrees),
    TableQuery[UserTable].forceInsertAll(employees ++ students),
    TableQuery[SemesterTable].forceInsertAll(semesters),
    TableQuery[CourseTable].forceInsertAll(courses),
    TableQuery[LabworkTable].forceInsertAll(labworks),
    TableQuery[RoomTable].forceInsertAll(rooms),
    TableQuery[ReportCardEntryTable].forceInsertAll(cards)
  )

  override protected val dao: ReportCardEntryTypeDao = app.injector.instanceOf(classOf[ReportCardEntryTypeDao])

  override protected def bindings: Seq[GuiceableModule] = Seq.empty
}
