package dao

import java.util.UUID

import dao.AbstractDaoSpec._
import models._
import org.joda.time.{DateTime, LocalDate, LocalTime}
import slick.dbio.DBIO
import slick.lifted.TableQuery
import store._
import slick.jdbc.PostgresProfile.api._
import utils.LwmDateTime._

final class ReportCardRetryDaoSpec extends AbstractExpandableDaoSpec[ReportCardRetryTable, ReportCardRetryDb, ReportCardRetry] with ReportCardRetryDao {
  import scala.util.Random.nextBoolean

  private lazy val privateLabworks = labworks.take(4)
  private lazy val reportCardEntries = populateReportCardEntries(20, 8, withRescheduledAndRetry = false)(privateLabworks, students)

  private def retry(reportCardEntryId: UUID, i: Int): ReportCardRetryDb = {
    val uuid = UUID.randomUUID
    val rDate = LocalDate.now.plusDays(i)
    val rStart = LocalTime.now.plusHours(i)
    val rEnd = rStart.plusHours(1)
    val entryTypes = randomReportCardEntryTypes(None, Some(uuid))

    ReportCardRetryDb(reportCardEntryId, rDate.sqlDate, rStart.sqlTime, rEnd.sqlTime, randomRoom.id, entryTypes, id = uuid)
  }

  private def retries(cards: List[ReportCardEntryDb]): List[ReportCardRetryDb] = {
    val ints = 1 until cards.size + 1

    cards.zip(ints).map {
      case (card, int) => retry(card.id, int)
    }
  }

  override protected val toAdd: List[ReportCardRetryDb] = retries(reportCardEntries.tail.drop(60))

  override protected val numberOfUpdates = 15

  override protected val numberOfDeletions = 15

  override protected def update(toUpdate: List[ReportCardRetryDb]): List[ReportCardRetryDb] = toUpdate.map { retry =>
    val maybeNewEntryTypes = if (nextBoolean)
      retry.entryTypes.map(t => t.copy(
        bool = if (nextBoolean) None else t.bool.map(b => !b),
        int = if (nextBoolean) t.int + 1 else 0
      ))
    else
      retry.entryTypes

    retry.copy(room = randomRoom.id, reason = Some(s"reason ${DateTime.now}"), entryTypes = maybeNewEntryTypes)
  }

  override protected def atom(dbModel: ReportCardRetryDb): PostgresReportCardRetryAtom = PostgresReportCardRetryAtom(
    dbModel.date.localDate,
    dbModel.start.localTime,
    dbModel.end.localTime,
    rooms.find(_.id == dbModel.room).get.toLwmModel,
    dbModel.entryTypes.map(_.toLwmModel),
    dbModel.reason,
    dbModel.id
  )

  override protected def expanderSpecs(dbModel: ReportCardRetryDb, isDefined: Boolean): DBIOAction[Unit, NoStream, Effect.Read] = DBIO.seq(
    entryTypeQuery.filter(_.reportCardRetry === dbModel.id).result.map { entryTypes =>
      entryTypes.toSet shouldBe (if (isDefined) dbModel.entryTypes else Set.empty)
    }
  )

  override protected def name = "reportCardRetryDaoSpec"

  override protected val dbEntity: ReportCardRetryDb = retry(reportCardEntries.head.id, 0).copy(entryTypes = Set.empty)

  override protected val invalidDuplicateOfDbEntity: ReportCardRetryDb = dbEntity

  override protected val invalidUpdateOfDbEntity: ReportCardRetryDb = dbEntity.copy(reportCardEntry = reportCardEntries.last.id)

  override protected val validUpdateOnDbEntity: ReportCardRetryDb = dbEntity.copy(room = randomRoom.id, reason = Some("reason"), date = dbEntity.date.localDate.plusWeeks(1).sqlDate)

  override protected val dbEntities: List[ReportCardRetryDb] = retries(reportCardEntries.tail.take(60))

  override protected val lwmEntity: PostgresReportCardRetry = dbEntity.toLwmModel

  override protected val lwmAtom: PostgresReportCardRetryAtom = atom(dbEntity)

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
