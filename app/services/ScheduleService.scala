package services

import java.util.UUID

import models.Group
import models.schedule.{ScheduleEntry, Schedule, Timetable}
import org.joda.time.DateTime
import store.SesameRepository
import utils.Evaluation._
import utils.TypeClasses._
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

case class Evaluation(value: Int, conflicts: List[Conflict])

trait ScheduleServiceLike {

  def applyBlacklist(timetable: Timetable): Timetable

  def populate(times: Int, timetable: Timetable, groups: Set[Group]): Vector[ScheduleG]

  def mutate(schedule: ScheduleG): ScheduleG

  def mutateDestructive(schedule: ScheduleG, conflicts: Vector[Conflict]): ScheduleG

  def crossover(left: ScheduleG, right: ScheduleG): (ScheduleG, ScheduleG)

  def evaluate(schedule: ScheduleG, appointments: Int): Evaluation
}

class ScheduleService(private val repository: SesameRepository) extends ScheduleServiceLike { self =>

  implicit val dateOrd: Ordering[DateTime] = new Ordering[DateTime] {
    override def compare(x: DateTime, y: DateTime): Int = x.compareTo(y)
  }

  def eval: EvalE[ScheduleG, Conflict, Int] = EvalE.instance[ScheduleG, Conflict, Int](s => evaluate2(s, appointments))
  def mut: MutateE[ScheduleG, Conflict, Int] = MutateE.instance[ScheduleG, Conflict, Int]((s, e) => self.mutate(s))
  def mutDest: MutateE[ScheduleG, Conflict, Int] = MutateE.instance[ScheduleG, Conflict, Int]((s, e) => self.mutateDestructive(s, e.err.toVector))
  def cross: CrossE[ScheduleG, Conflict, Int] = CrossE.instance[ScheduleG, Conflict, Int] {
    case ((s1, _), (s2, _)) => crossover(s1, s2)
  }

  def evaluate2(schedule: ScheduleG, appointments: Int): utils.Evaluation[Conflict, Int] = {
    def collide(left: ScheduleEntryG, right: ScheduleEntryG): Boolean = {
      (left.date.isEqual(right.date) && left.day.isEqual(right.day)) && left.start.isEqual(right.start) || (right.start.isAfter(left.start) && right.start.isBefore(left.end))
    }

    val integrity = schedule.entries.groupBy(_.group) forall {
      case (_, ss) => ss.size == appointments
    }

    //if it's consistent, then calculate it's proper weight, otherwise give it the maximum possible value
    if (integrity) {
      // /check entries against each already existing schedule
      scheduleFor(schedule.labwork)
        .flatMap(_.entries)
        .map(e => (e, schedule.entries.find(f => collide(e, f))))
        .foldLeft(evaluationV[Conflict, Int](0)) {
          case (eval, (ee, Some(e))) =>
            val m = ee.group.members.intersect(e.group.members)
            val c = Conflict(e, m.toVector, e.group)
            eval.map(_ + m.size).mapErrWhole(_ :+ c)
        }
    } else {
      evaluationV[Conflict, Int](Integer.MAX_VALUE)
    }
  }

  //TODO: Returns the amount of required appointments
  private def appointments: Int = ???

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

  //TODO: expand to UUID -> Labwork -> SemesterId -> ScheduleG's
  private def scheduleFor(labwork: UUID): List[ScheduleG] = ???

  //TODO: Schedule for is external dependency, that gets retrieved a priori
  override def evaluate(schedule: ScheduleG, appointments: Int): Evaluation = {
    def collide(left: ScheduleEntryG, right: ScheduleEntryG): Boolean = {
      (left.date.isEqual(right.date) && left.day.isEqual(right.day)) && left.start.isEqual(right.start) || (right.start.isAfter(left.start) && right.start.isBefore(left.end))
    }

    // /check entries against each already existing schedule
    val conflicts = scheduleFor(schedule.labwork).flatMap(_.entries).map(e => (e, schedule.entries.find(f => collide(e, f)))).foldLeft(List.empty[Conflict]) {
      case (list, (ee, Some(e))) =>
        val m = ee.group.members.intersect(e.group.members)
        val c = Conflict(e, m.toVector, e.group)
        list :+ c
    }

    /*conflicts.groupBy(_.group).foldLeft(Evaluation(0, List.empty[Conflict])) {
      case (e, (group, list)) => Evaluation(e.value + list.size, e.conflicts ::: list)
    }*/

    //check integrity of group-appointment relation
    val integrity = schedule.entries.groupBy(_.group) forall {
      case (_, ss) => ss.size == appointments
    }

    val factor = if (integrity) 1000 else 0

    Evaluation(conflicts.foldRight(factor)(_.member.size + _), conflicts)
  }
}
