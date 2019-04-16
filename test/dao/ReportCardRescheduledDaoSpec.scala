package dao

import java.util.UUID

import database._
import models.{ReportCardRescheduledAtom, ReportCardRescheduledLike}
import org.joda.time.{LocalDate, LocalTime}
import play.api.inject.guice.GuiceableModule
import slick.dbio.DBIO
import slick.jdbc.PostgresProfile.api._
import slick.lifted.TableQuery
import utils.date.DateTimeOps._

final class ReportCardRescheduledDaoSpec extends AbstractDaoSpec[ReportCardRescheduledTable, ReportCardRescheduledDb, ReportCardRescheduledLike] {

  import AbstractDaoSpec._

  private val amount = 50
  private lazy val privateLabworks = labworks.take(4)
  private lazy val reportCardEntries = populateReportCardEntries(amount, 8, withRescheduledAndRetry = true)(privateLabworks, students)

  def rescheduled(reportCardEntryId: UUID, i: Int): ReportCardRescheduledDb = {
    val rDate = LocalDate.now.plusDays(i)
    val rStart = LocalTime.now.plusHours(i)
    val rEnd = rStart.plusHours(1)

    ReportCardRescheduledDb(reportCardEntryId, rDate.sqlDate, rStart.sqlTime, rEnd.sqlTime, randomRoom.id)
  }

  override protected def name = "reportCardRescheduleSpec"

  override protected val dbEntity: ReportCardRescheduledDb = rescheduled(reportCardEntries.head.id, 0)

  override protected val invalidDuplicateOfDbEntity: ReportCardRescheduledDb = dbEntity

  override protected val invalidUpdateOfDbEntity: ReportCardRescheduledDb = dbEntity.copy(reportCardEntry = reportCardEntries.last.id)

  override protected val validUpdateOnDbEntity: ReportCardRescheduledDb = dbEntity.copy(
    room = randomRoom.id, reason = Some("reason"), date = dbEntity.date.localDate.plusWeeks(1).sqlDate
  )

  override protected val dbEntities: List[ReportCardRescheduledDb] = {
    val entries = reportCardEntries.drop(1)
    (0 until amount).map(i => rescheduled(entries(i).id, i)).toList
  }

  override protected val lwmAtom: ReportCardRescheduledAtom = ReportCardRescheduledAtom(
    dbEntity.date.localDate,
    dbEntity.start.localTime,
    dbEntity.end.localTime,
    rooms.find(_.id == dbEntity.room).get.toUniqueEntity,
    dbEntity.reason,
    dbEntity.id
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

  override protected val dao: ReportCardRescheduledDao = app.injector.instanceOf(classOf[ReportCardRescheduledDao])

  override protected def bindings: Seq[GuiceableModule] = Seq.empty
}
