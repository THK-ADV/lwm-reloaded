package services

import java.util.UUID

import models.Group
import models.schedule.{ScheduleEntry, Schedule, Timetable}
import org.joda.time.DateTime

import scala.language.higherKinds

case class Conflict(entry: ScheduleEntryG, member: Vector[UUID], group: Group)

case class ScheduleG(labwork: UUID, entries: Set[ScheduleEntryG], id: UUID) {

  override def equals(that: scala.Any): Boolean = that match {
    case ScheduleG(l, e, i) =>
      labwork == l && entries.zip(e).forall(z => z._1 == z._2) && id == i
    case _ => false
  }
}

case class ScheduleEntryG(start: DateTime, end: DateTime, day: DateTime, date: DateTime, room: UUID, supervisor: UUID, group: Group, id: UUID) {

  override def equals(that: scala.Any): Boolean = that match {
    case ScheduleEntryG(s, e, dy, da, r, su, g, i) =>
      start.isEqual(s) && end.isEqual(e) && day.isEqual(dy) && date.isEqual(da) && room == r && su == su && group == g && id == i
    case _ => false
  }
}

trait ScheduleServiceLike {

  def applyBlacklist(timetable: Timetable): Timetable

  def populate(times: Int, timetable: Timetable, groups: Set[Group]): Vector[ScheduleG]

  def mutate(schedule: ScheduleG): ScheduleG

  def mutateDestructive(schedule: ScheduleG, conflicts: Vector[Conflict]): ScheduleG

  def crossover(left: ScheduleG, right: ScheduleG): (ScheduleG, ScheduleG)
}

class ScheduleService extends ScheduleServiceLike {

  implicit val dateOrd: Ordering[DateTime] = new Ordering[DateTime] {
    override def compare(x: DateTime, y: DateTime): Int = x.compareTo(y)
  }

  override def populate(times: Int, timetable: Timetable, groups: Set[Group]): Vector[ScheduleG] = (0 until times).map(_ => populate(timetable, groups)).toVector

  private def populate(timetable: Timetable, groups: Set[Group]): ScheduleG = {
    import scala.util.Random._

    val entries = timetable.entries.toVector.sortBy(_.date).grouped(groups.size).flatMap(_.zip(shuffle(groups)).map {
      case (t, group) => ScheduleEntryG(t.start, t.end, t.day, t.date, t.room, t.supervisor, group, ScheduleEntry.randomUUID)
    }).toSet

    ScheduleG(timetable.labwork, entries, Schedule.randomUUID)
  }

  override def applyBlacklist(timetable: Timetable): Timetable = timetable

  override def mutate(schedule: ScheduleG): ScheduleG = {
    import scala.util.Random._

    implicit val groups = schedule.entries.toVector.map(_.group)
    def randomGroup(implicit groups: Vector[Group]): Group = groups(nextInt(groups.size))

    (randomGroup, randomGroup) match {
      case (toSwap, withValue) if toSwap != withValue =>
        val mutated = schedule.entries.toVector.map(_.group).map {
          case `toSwap` => withValue
          case `withValue` => toSwap
          case x => x
        }

        val swapped = schedule.entries.zip(mutated).map {
          case (entry, group) => ScheduleEntryG(entry.start, entry.end, entry.day, entry.date, entry.room, entry.supervisor, group, entry.id)
        }

        ScheduleG(schedule.labwork, swapped, schedule.id)

      case _ => mutate(schedule)
    }
  }

  override def mutateDestructive(schedule: ScheduleG, conflicts: Vector[Conflict]): ScheduleG = {
    conflicts.foldRight(schedule) {
      case (c, s) =>
        val g = s.entries.map(_.group).toVector.sortBy(_.label).last // TODO: choose another group by time
        val x = g.members.take(c.member.size).toList

        val ncg = c.group.members.foldLeft((Set[UUID](), x)) {
          case ((set, xs), problem) if c.member.contains(problem) => (set + xs.head, xs.tail)
          case ((set, xs), ok) => (set + ok, xs)
        }._1

        val z = c.group.members.diff(ncg).zip(ncg.diff(c.group.members)).map(_.swap)
        val ng = z.foldLeft(g.members.toVector) {
          case (members, (m1, m2)) => members.updated(members.indexOf(m1), m2)
        }.toSet

        val a = Group(c.group.label, c.group.labwork, ncg, c.group.id)
        val b = Group(g.label, g.labwork, ng, g.id)

        val ne = s.entries.map {
          case ua if ua.group.id == a.id => ScheduleEntryG(ua.start, ua.end, ua.day, ua.date, ua.room, ua.supervisor, a, ua.id)
          case ub if ub.group.id == b.id => ScheduleEntryG(ub.start, ub.end, ub.day, ub.date, ub.room, ub.supervisor, b, ub.id)
          case ok => ok
        }

        ScheduleG(s.labwork, ne, s.id)
    }
  }

  override def crossover(left: ScheduleG, right: ScheduleG): (ScheduleG, ScheduleG) = {
    import scala.util.Random._

    def sameDate(left: ScheduleEntryG, right: ScheduleEntryG): Boolean = {
      left.date.isEqual(right.date) && left.day.isEqual(right.day) && left.start.isEqual(right.start) && left.end.isEqual(right.end)
    }

    val size = left.entries.map(_.group).size
    val pairs = left.entries.toVector.sortBy(_.date).zip(right.entries.toVector.sortBy(_.date)).grouped(size).toVector
    val week = nextInt(pairs.size)

    val toCross = shuffle(pairs(week)).take(2).unzip._1
    val r = pairs(week).unzip._2
    val l = pairs(week).unzip._1
    val a = r(l.indexOf(toCross.head))
    val b = r(l.indexOf(toCross.last))

    val crossover = toCross.zip(Vector(b, a))
    val rCrossover = toCross.zip(Vector(a, b))

    val crossed = pairs.flatMap {
      case swap if swap.exists(p => rCrossover.contains(p)) =>
        val u = swap.unzip

        val l = u._1.map (ss => (ss, crossover.find(cs => sameDate(ss, cs._1)))).foldLeft(Vector[ScheduleEntryG]()) {
          case (vec, (e, Some((_, withEntry)))) => vec :+ ScheduleEntryG(withEntry.start, withEntry.end, withEntry.day, withEntry.date, withEntry.room, withEntry.supervisor, e.group, e.id)
          case (vec, (e, None)) => vec :+ e
        }

        val r = u._2.map (ss => (ss, crossover.find(cs => sameDate(cs._2, ss)))).foldLeft(Vector[ScheduleEntryG]()) {
          case (vec, (e, Some((withEntry, _)))) => vec :+ ScheduleEntryG(withEntry.start, withEntry.end, withEntry.day, withEntry.date, withEntry.room, withEntry.supervisor, e.group, e.id)
          case (vec, (e, None)) => vec :+ e
        }

        l.zip(r)
      case ok => ok
    }.unzip

    (ScheduleG(left.labwork, crossed._1.toSet, left.id), ScheduleG(right.labwork, crossed._2.toSet, right.id))
  }
}