package service

import java.util.UUID

import dao.helper.TableFilter
import dao.{ReportCardEntryDao, ScheduleEntryDao}
import database.{ReportCardEntryDb, ReportCardEntryTypeDb, ReportCardRescheduledDb, ReportCardRetryDb}
import javax.inject.Inject
import models._
import models.assignment.AssignmentEntry

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

object ReportCardEntryService {

  def generateReportCardEntries(scheduleEntries: Seq[ScheduleEntryAtom], assignmentEntries: Seq[AssignmentEntry]): Try[Seq[ReportCardEntry]] = {
    def sortedAppointments(student: UUID): Seq[ScheduleEntryAtom] = {
      import utils.date.DateTimeOps._
      scheduleEntries.filter(_.group.members.contains(student)).sortBy(toLocalDateTime)
    }

    def students: Seq[UUID] =
      scheduleEntries.flatMap(_.group.members).distinct

    def go: Seq[ReportCardEntry] = {
      val assignments = assignmentEntries.sortBy(_.index)

      for {
        student <- students
        appointments = sortedAppointments(student)
        (s, a) <- appointments.zip(assignments)
      } yield {
        ReportCardEntry(
          student,
          a.labwork,
          a.label,
          s.date,
          s.start,
          s.end,
          s.room.id,
          Set.empty/*a.types.map(t => ReportCardEntryType(t.label))*/, // TODO
          a.index
        )
      }
    }

    (scheduleEntries.size, assignmentEntries.size) match {
      case (a, b) if a == 0 || b == 0 =>
        Failure(new Throwable("can't generate report card entries if either schedule entries or assignment are empty"))
      case (a, b) if a % b == 0 =>
        Success(go)
      case _ =>
        Failure(new Throwable(s"can't distribute ${assignmentEntries.size} assignment entries over ${scheduleEntries.size} schedule entries"))
    }
  }
}

trait ReportCardEntryService {

  import ReportCardEntryService._
  import utils.date.DateTimeOps.{LocalDateConverter, LocalTimeConverter}

  implicit def ctx: ExecutionContext

  def dao: ReportCardEntryDao

  def scheduleEntryDao: ScheduleEntryDao

  def assignmentEntryService: AssignmentEntryService

  def generate(labwork: UUID): Future[Seq[ReportCardEntry]] = {
    for {
      scheduleEntries <- scheduleEntryDao.get(List(TableFilter.labworkFilter(labwork)))
      assignmentEntries <- assignmentEntryService.dao.get(List(TableFilter.labworkFilter(labwork)), atomic = false)
      reportCardEntries <- Future.fromTry(generateReportCardEntries(scheduleEntries.map(_.asInstanceOf[ScheduleEntryAtom]), assignmentEntries.map(_.asInstanceOf[AssignmentEntry])))
      _ <- dao.createMany(reportCardEntries.map(toDb).toList)
    } yield reportCardEntries
  }

  private def toDb(x: ReportCardEntry): ReportCardEntryDb = ReportCardEntryDb(
    x.student,
    x.labwork,
    x.label,
    x.date.sqlDate,
    x.start.sqlTime,
    x.end.sqlTime,
    x.room,
    x.entryTypes.map(toEntryTypeDb(x.id)),
    x.assignmentIndex,
    x.rescheduled.map(toRescheduledDb(x.id)),
    x.retry.map(toRetryDb(x.id)),
    id = x.id
  )

  private def toEntryTypeDb(reportCardEntry: UUID)(t: ReportCardEntryType) = ReportCardEntryTypeDb(
    Some(reportCardEntry),
    None,
    t.entryType,
    t.bool,
    t.int,
    id = t.id
  )

  private def toRescheduledDb(reportCardEntry: UUID)(r: ReportCardRescheduled) = ReportCardRescheduledDb(
    reportCardEntry,
    r.date.sqlDate,
    r.start.sqlTime,
    r.end.sqlTime,
    r.room,
    r.reason,
    id = r.id
  )

  private def toRetryDb(reportCardEntry: UUID)(r: ReportCardRetry) = ReportCardRetryDb(
    reportCardEntry,
    r.date.sqlDate,
    r.start.sqlTime,
    r.end.sqlTime,
    r.room,
    r.entryTypes.map(toEntryTypeDb(reportCardEntry)),
    r.reason,
    id = r.id
  )
}

final class ReportCardEntryServiceImpl @Inject()(
  val ctx: ExecutionContext,
  val dao: ReportCardEntryDao,
  val scheduleEntryDao: ScheduleEntryDao,
  val assignmentEntryService: AssignmentEntryService
) extends ReportCardEntryService
