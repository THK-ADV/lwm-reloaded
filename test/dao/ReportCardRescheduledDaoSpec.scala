package dao

import java.util.UUID

import models.{PostgresReportCardRescheduled, PostgresReportCardRescheduledAtom, ReportCardRescheduled, ReportCardRescheduledDb}
import slick.dbio.DBIO
import slick.lifted.TableQuery
import store._
import slick.jdbc.PostgresProfile.api._
import utils.LwmDateTime._
import org.joda.time.{LocalDate, LocalTime}

final class ReportCardRescheduledDaoSpec extends AbstractDaoSpec[ReportCardRescheduledTable, ReportCardRescheduledDb, ReportCardRescheduled] with ReportCardRescheduledDao  {
  import dao.AbstractDaoSpec._

  private val amount = 50
  private lazy val privateLabworks = labworks.take(4)
  private lazy val reportCardEntries = populateReportCardEntries(amount, 8, withRescheduledAndRetry = false)(privateLabworks, students)

  def rescheduled(reportCardEntryId: UUID, i: Int): ReportCardRescheduledDb = {
    val rDate = LocalDate.now.plusDays(i)
    val rStart = LocalTime.now.plusHours(i)
    val rEnd = rStart.plusHours(1)

    ReportCardRescheduledDb(reportCardEntryId, rDate.sqlDate, rStart.sqlTime, rEnd.sqlTime, randomRoom.id)
  }

  "A ReportCardRescheduledDaoSpec also" should {
    "filter rescheduled properly" in {
      await(get(List(ReportCardRescheduledLabworkFilter(privateLabworks.head.id.toString)))).size < amount shouldBe true
    }
  }

  override protected def name = "reportCardRescheduleSpec"

  override protected val dbEntity: ReportCardRescheduledDb = rescheduled(reportCardEntries.head.id, 0)

  override protected val invalidDuplicateOfDbEntity: ReportCardRescheduledDb = dbEntity

  override protected val invalidUpdateOfDbEntity: ReportCardRescheduledDb = dbEntity.copy(reportCardEntry = reportCardEntries.last.id)

  override protected val validUpdateOnDbEntity: ReportCardRescheduledDb = dbEntity.copy(room = randomRoom.id, reason = Some("reason"), date = dbEntity.date.localDate.plusWeeks(1).sqlDate)

  override protected val dbEntities: List[ReportCardRescheduledDb] = (0 until amount).map(i => rescheduled(reportCardEntries(i).id, i)).toList

  override protected val lwmEntity: PostgresReportCardRescheduled = dbEntity.toLwmModel

  override protected val lwmAtom: PostgresReportCardRescheduledAtom = PostgresReportCardRescheduledAtom(
    lwmEntity.date,
    lwmEntity.start,
    lwmEntity.end,
    rooms.find(_.id == lwmEntity.room).head.toLwmModel,
    lwmEntity.reason,
    lwmEntity.id
  )

  override protected val dependencies = DBIO.seq(
    TableQuery[DegreeTable].forceInsertAll(degrees),
    TableQuery[UserTable].forceInsertAll(employees ++ students),
    TableQuery[SemesterTable].forceInsertAll(semesters),
    TableQuery[CourseTable].forceInsertAll(courses),
    TableQuery[LabworkTable].forceInsertAll(labworks),
    TableQuery[RoomTable].forceInsertAll(rooms),
    TableQuery[ReportCardEntryTable].forceInsertAll(reportCardEntries)
  )
}
