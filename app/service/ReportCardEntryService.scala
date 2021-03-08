package service

import dao.helper.TableFilter
import dao.{AnnotationDao, ReportCardEntryDao, ScheduleEntryDao}
import database._
import models._
import org.joda.time.{LocalDate, LocalTime}
import slick.jdbc.JdbcProfile

import java.util.UUID
import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

object ReportCardEntryService {

  case class ReportCardEntryDescription(label: String, date: LocalDate, start: LocalTime, end: LocalTime, room: UUID, entryTypes: Set[String])

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
          a.types.map(t => ReportCardEntryType(t.entryType)),
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

  def liftDescriptions(descriptions: List[ReportCardEntryDescription], student: UUID, labwork: UUID, currentMaxIndex: Int): Seq[ReportCardEntry] = {
    def go(e: ReportCardEntryDescription, index: Int) = ReportCardEntry(
      student,
      labwork,
      e.label,
      e.date,
      e.start,
      e.end,
      e.room,
      e.entryTypes.map(ReportCardEntryType(_)),
      index,
    )

    val lastIndex = currentMaxIndex + 1 // TODO extended ReportCards should not have an index, since they are on top or special in some way
    descriptions.zipWithIndex.map(d => go(d._1, lastIndex + d._2))
  }
}

trait ReportCardEntryService {

  val profile: JdbcProfile

  import ReportCardEntryService._
  import TableFilter.{labworkByReportCardEntryFilter, userByReportCardEntryFilter}
  import profile.api._
  import utils.date.DateTimeOps.{LocalDateConverter, LocalTimeConverter}

  implicit def ctx: ExecutionContext

  def dao: ReportCardEntryDao

  def annotationDao: AnnotationDao

  def scheduleEntryDao: ScheduleEntryDao

  def assignmentEntryService: AssignmentEntryService

  def withAnnotationCountInLabwork(
    filter: List[ReportCardEntryTable => Rep[Boolean]],
    atomic: Boolean,
    validOnly: Boolean,
    lastModified: Option[String]
  ) = {
    def annotationFor(e: ReportCardEntryLike) =
      annotationDao.count(List(userByReportCardEntryFilter(e.studentId), labworkByReportCardEntryFilter(e.labworkId)))

    for {
      entries <- dao.get(filter, atomic, validOnly, lastModified)
      withAnnotations <- Future.sequence(entries.map(e => annotationFor(e).map((e, _))))
    } yield withAnnotations
  }

  def rescheduleCandidates(course: UUID, semester: UUID) = {
    import ReportCardEntryDao.precisedAppointmentFilter
    import utils.date.DateTimeOps.{SqlDateConverter, SqlTimeConverter}

    val query = (for {
      x <- dao.filterValidOnly(x => x.memberOfCourse(course) && x.inSemester(semester))
      r <- x.roomFk
    } yield (x.date, x.start, x.end, r)).distinct

    val query2 = for {
      (date, start, end, room) <- query
      xs <- dao.filterValidOnly(x =>
        precisedAppointmentFilter(course, semester, date, start, end, room)(x) // TODO remove rescheduled outs, add rescheduled ins
      )
    } yield (date, start, end, room, xs.id)

    val action = query2.result.map(_.groupBy(t => (t._1.localDate, t._2.localTime, t._3.localTime, t._4.toUniqueEntity)).map {
      case ((date, start, end, room), members) =>
        (date, start, end, room, members.size)
    })

    dao.db.run(action)
  }

  def generate(labwork: UUID): Future[Seq[ReportCardEntry]] = {
    for {
      scheduleEntries <- scheduleEntryDao.get(List(TableFilter.labworkFilter(labwork)))
      assignmentEntries <- assignmentEntryService.dao.get(List(TableFilter.labworkFilter(labwork)), atomic = false)
      reportCardEntries <- Future.fromTry(generateReportCardEntries(scheduleEntries.map(_.asInstanceOf[ScheduleEntryAtom]), assignmentEntries.map(_.asInstanceOf[AssignmentEntry])))
      _ <- dao.createMany(reportCardEntries.map(toDb).toList)
    } yield reportCardEntries
  }

  def extendBy(labwork: UUID, descriptions: List[ReportCardEntryDescription], atomic: Boolean): Future[Seq[ReportCardEntryLike]] = {
    def lift(cards: Seq[ReportCardEntry]): Seq[ReportCardEntry] = cards.headOption match {
      case Some(card) =>
        liftDescriptions(descriptions, card.student, card.labwork, cards.maxBy(_.assignmentIndex).assignmentIndex)
      case None =>
        Nil
    }

    for {
      cards <- dao.get(List(TableFilter.labworkFilter(labwork)), atomic = false)
      lifted = cards
        .map(_.asInstanceOf[ReportCardEntry])
        .groupBy(_.student)
        .flatMap(a => lift(a._2))
      _ <- dao.createMany(lifted.map(toDb).toList)
      all <- dao.get(List(TableFilter.labworkFilter(labwork)), atomic)
    } yield all
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
    id = x.id
  )

  private def toEntryTypeDb(reportCardEntry: UUID)(t: ReportCardEntryType) = ReportCardEntryTypeDb(
    reportCardEntry,
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
}

final class ReportCardEntryServiceImpl @Inject()(
  val ctx: ExecutionContext,
  val dao: ReportCardEntryDao,
  val annotationDao: AnnotationDao,
  val scheduleEntryDao: ScheduleEntryDao,
  val assignmentEntryService: AssignmentEntryService,
  val profile: JdbcProfile
) extends ReportCardEntryService
