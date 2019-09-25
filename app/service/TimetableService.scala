package service

import java.util.UUID

import dao.{BlacklistDao, TimetableDao}
import database.{BlacklistDb, TimetableDb}
import models.helper.{TimetableDateEntry, Weekday}
import models.{helper, _}
import org.joda.time.{Interval, Weeks}
import utils.date.DateTimeOps._

import scala.concurrent.{ExecutionContext, Future}

object TimetableService { // TODO DI

  def withoutBlacklists(entries: Vector[TimetableDateEntry], blacklists: Vector[Blacklist]): Vector[TimetableDateEntry] = {
    entries.filterNot { e =>
      val entry = new Interval(e.date.toDateTime(e.start), e.date.toDateTime(e.end))

      blacklists.exists { b =>
        val blacklist = new Interval(b.date.toDateTime(b.start), b.date.toDateTime(b.end))

        entry overlaps blacklist
      }
    }
  }

  @scala.annotation.tailrec
  def extrapolateTimetableByWeeks(timetable: Timetable,
    weeks: Weeks,
    blacklists: Vector[Blacklist],
    assignmentPlan: AssignmentPlan,
    groupSize: Int): Vector[TimetableDateEntry] = {
    val appointments = assignmentPlan.entries.size * groupSize
    val schemaWeek = timetable.entries.toVector.map { entry =>
      val weekday = Weekday.toDay(entry.dayIndex)
      helper.TimetableDateEntry(weekday, weekday.sync(timetable.start), entry.start, entry.end, entry.room, entry.supervisor)
    }

    val extrapolated = (0 until weeks.getWeeks).foldLeft(Vector.empty[TimetableDateEntry]) {
      case (vec, week) =>
        val nextWeek = schemaWeek.map(e => TimetableDateEntry(e.weekday, e.date.plusWeeks(week), e.start, e.end, e.room, e.supervisor))
        vec ++ nextWeek
    }

    val filtered = withoutBlacklists(extrapolated, blacklists)

    takeAppointments(filtered, assignmentPlan, groupSize) match {
      case enough if enough.size >= appointments => enough
      case _ => extrapolateTimetableByWeeks(timetable, weeks plus Weeks.ONE, blacklists, assignmentPlan, groupSize)
    }
  }

  private def takeAppointments(entries: Vector[TimetableDateEntry], assignmentPlan: AssignmentPlan, groupSize: Int): Vector[TimetableDateEntry] = {
    val sorted = entries.sortBy(toLocalDateTime)
    val initial = sorted.take(groupSize)
    val remaining = sorted.drop(groupSize)

    assignmentPlan.entries.toVector.sortBy(_.index).drop(1).foldLeft((remaining, initial)) {
      case ((e, vec), ae) =>
        val skip = groupSize * (ae.duration - 1)
        val remain = e.drop(skip)

        (remain.drop(groupSize), vec ++ remain.take(groupSize))
    }._2
  }

  def removeBlacklistFromTimetable(
    blacklistDao: BlacklistDao,
    timetableDao: TimetableDao
  )(blacklistId: UUID, timetableId: UUID)(implicit executionContext: ExecutionContext): Future[(Timetable, Option[Blacklist])] = {
    def isLocalBlacklist(blacklist: Blacklist, timetable: Timetable) =
      if (timetable.localBlacklist.contains(blacklistId))
        Future.unit
      else
        Future.failed(new Throwable(s"blacklist $blacklist is not a local blacklist in $timetable"))

    def remove(blacklist: Blacklist, timetable: Timetable): Future[(TimetableDb, Option[BlacklistDb])] = {
      def removeRelationShip() = {
        import utils.date.DateTimeOps.LocalDateConverter

        def toTimetableDb = TimetableDb(
          timetable.labwork,
          timetable.entries,
          timetable.start.sqlDate,
          timetable.localBlacklist - blacklist.id,
          id = timetable.id
        )

        timetableDao update toTimetableDb
      }

      if (blacklist.global)
        for {
          r <- removeRelationShip()
        } yield (r, None)
      else
        for {
          r <- removeRelationShip()
          b <- blacklistDao.delete(blacklistId)
        } yield (r, Some(b))
    }

    for {
      maybeBlacklist <- blacklistDao.getSingle(blacklistId, atomic = false) if maybeBlacklist.isDefined
      maybeTimetable <- timetableDao.getSingle(timetableId, atomic = false) if maybeTimetable.isDefined
      blacklist = maybeBlacklist.get
      timetable = maybeTimetable.get.asInstanceOf[Timetable]

      _ <- isLocalBlacklist(blacklist, timetable)
      (t, b) <- remove(blacklist, timetable)
    } yield (t.toUniqueEntity, b.map(_.toUniqueEntity))
  }
}