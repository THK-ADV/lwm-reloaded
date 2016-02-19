package services

import java.util.UUID
import models.{AssignmentPlan, Group}
import models.schedule._
import org.joda.time.{LocalDate, LocalTime}
import utils.{Genesis, Gen}
import utils.TypeClasses._
import scala.language.higherKinds
import scala.util.Random

// TODO: refactor out of file
case class Conflict(entry: ScheduleEntryG, member: Vector[UUID], group: Group) // TODO entry with labwork

case class Evaluation(value: Int, conflicts: List[Conflict])

case class ScheduleG(labwork: UUID, entries: Set[ScheduleEntryG], id: UUID) {

  override def equals(that: scala.Any): Boolean = that match {
    case ScheduleG(l, e, i) =>
      import TimetableDateEntry._

      labwork == l && entries.toVector.sortBy(toLocalDateTime).zip(e.toVector.sortBy(toLocalDateTime)).forall(z => z._1 == z._2) && id == i
    case _ => false
  }
}

case class ScheduleEntryG(start: LocalTime, end: LocalTime, date: LocalDate, room: UUID, supervisor: UUID, group: Group, id: UUID) {

  override def equals(that: scala.Any): Boolean = that match {
    case ScheduleEntryG(s, e, d, r, su, g, i) =>
      start.isEqual(s) && end.isEqual(e) && date.isEqual(d) && room == r && su == su && group == g && id == i
    case _ => false
  }
}

trait ScheduleServiceLike {

  def populate(times: Int, timetable: Timetable, assignmentPlan: AssignmentPlan, groups: Set[Group]): Vector[ScheduleG]

  def mutate(schedule: ScheduleG): ScheduleG

  def mutateDestructive(schedule: ScheduleG, conflicts: Vector[Conflict]): ScheduleG

  def crossover(left: (ScheduleG, List[Conflict]), right: (ScheduleG, List[Conflict])): (ScheduleG, ScheduleG) // TODO Vector[Conflict]

  def evaluate(schedule: ScheduleG, appointments: Int, all: Vector[ScheduleG]): Evaluation

  def evaluate2(schedule: ScheduleG, appointments: Int, all: Vector[ScheduleG]): utils.Evaluation[Conflict, Int]
}

trait ScheduleGenesisServiceLike {

  def generate(timetable: Timetable, groups: Set[Group], assignmentPlan: AssignmentPlan, competitive: Vector[ScheduleG]): (Gen[ScheduleG, Conflict, Int], Int)
}

class ScheduleService(private val timetableService: TimetableServiceLike) extends ScheduleServiceLike with ScheduleGenesisServiceLike { self =>

  // TODO: refactor this weird functional foo
  def eval(all: Vector[ScheduleG], appts: Int): EvalE[ScheduleG, Conflict, Int] = EvalE.instance[ScheduleG, Conflict, Int](s => evaluate2(s, appts, all))
  def mut: MutateE[ScheduleG, Conflict, Int] = MutateE.instance[ScheduleG, Conflict, Int]((s, e) => self.mutate(s))
  def mutDest: MutateE[ScheduleG, Conflict, Int] = MutateE.instance[ScheduleG, Conflict, Int]((s, e) => self.mutateDestructive(s, e.err.toVector))
  def cross: CrossE[ScheduleG, Conflict, Int] = CrossE.instance[ScheduleG, Conflict, Int] {
    case ((s1, e1), (s2, e2)) => crossover((s1, e1.err), (s2, e2.err))
  }

  // TODO: get rid of that
  override def evaluate2(schedule: ScheduleG, appointments: Int, all: Vector[ScheduleG]): utils.Evaluation[Conflict, Int] = {
    val ev = evaluate(schedule, appointments, all)
    utils.Evaluation.evaluation(ev.value, ev.conflicts)
  }

  override def generate(timetable: Timetable, groups: Set[Group], assignmentPlan: AssignmentPlan, competitive: Vector[ScheduleG]): (Gen[ScheduleG, Conflict, Int], Int) = {
    val pop = populate(100, timetable, assignmentPlan, groups)

    implicit val evalF = eval(competitive, assignmentPlan.numberOfEntries)
    implicit val mutateF = mut
    implicit val crossF = cross
    import utils.TypeClasses.instances._
    import utils.Ops.MonadInstances.intM

    Genesis.measureByTaking[ScheduleG, Conflict, Int](pop, 200)
  }

  override def populate(times: Int, timetable: Timetable, assignmentPlan: AssignmentPlan, groups: Set[Group]): Vector[ScheduleG] = {
    val entries = timetableService.extrapolateEntries(timetable, assignmentPlan, groups)
    (0 until times).map(_ => populate(timetable, entries, assignmentPlan, groups)).toVector
  }

  private def populate(timetable: Timetable, entries: Set[TimetableDateEntry], assignmentPlan: AssignmentPlan, groups: Set[Group]): ScheduleG = {
    import scala.util.Random._
    import TimetableDateEntry._

    val sg = shuffle(groups.toVector)
    val scheduleEntries = entries.toVector.sortBy(toLocalDateTime).grouped(groups.size).toVector.flatMap(_.zip(sg).map {
      case (t, group) =>
        val o = TimetableDateEntry.organizer(t, timetable.entries)
        ScheduleEntryG(t.start, t.end, t.date, o.room, o.supervisor, group, ScheduleEntry.randomUUID)
    }).toSet

    ScheduleG(timetable.labwork, scheduleEntries, Schedule.randomUUID)
  }

  def randomGroup(implicit groups: Vector[Group]): Group = groups(Random.nextInt(groups.size))
  def randomAvoiding(g: Group)(implicit groups: Vector[Group]): Group = {
    val group = randomGroup
    if(group.id == g.id) randomAvoiding(g)
    else group
  }

  def replaceSchedule(s: ScheduleG)(f: ScheduleEntryG => ScheduleEntryG): ScheduleG = ScheduleG(s.labwork, s.entries.map(f), s.id)
  def swapWithin(s: ScheduleG)(left: Group, right: Group): ScheduleG = replaceSchedule(s) {
    case x if x.group.id == left.id => ScheduleEntryG(x.start, x.end, x.date, x.room, x.supervisor, right, x.id)
    case y if y.group.id == right.id => ScheduleEntryG(y.start, y.end, y.date, y.room, y.supervisor, left, y.id)
    case z => z
  }

   override def mutate(schedule: ScheduleG): ScheduleG = {
    implicit val groups = schedule.entries.map(_.group).toVector
    val group1 = randomGroup
    val group2 = randomAvoiding(group1)
    swapWithin(schedule)(group1, group2)
  }

  override def mutateDestructive(schedule: ScheduleG, conflicts: Vector[Conflict]): ScheduleG = {
    conflicts.foldRight(schedule) { // TODO ordering is broken
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
          case ua if ua.group.id == a.id => ScheduleEntryG(ua.start, ua.end, ua.date, ua.room, ua.supervisor, a, ua.id)
          case ub if ub.group.id == b.id => ScheduleEntryG(ub.start, ub.end, ub.date, ub.room, ub.supervisor, b, ub.id)
          case ok => ok
        }

        ScheduleG(s.labwork, ne, s.id)
    }
  }

  override def crossover(left: (ScheduleG, List[Conflict]), right: (ScheduleG, List[Conflict])): (ScheduleG, ScheduleG) = {
    (Random.shuffle(left._2), Random.shuffle(right._2)) match {
      case (h1 :: _, h2 :: _) =>
        val cl = swapWithin(left._1)(h1.group, randomAvoiding(h1.group)(left._1.entries.map(_.group).toVector))
        val cr = swapWithin(right._1)(h2.group, randomAvoiding(h2.group)(right._1.entries.map(_.group).toVector))

        (cl, cr)

      case _ => (left._1, right._1)
    }
  }
  override def evaluate(schedule: ScheduleG, appointments: Int, all: Vector[ScheduleG]): Evaluation = {
    def collide(left: ScheduleEntryG, right: ScheduleEntryG): Boolean = {
      left.date.isEqual(right.date) && left.start.isEqual(right.start) || (right.start.isAfter(left.start) && right.start.isBefore(left.end))
    }

    // /check entries against each already existing schedule
    val conflicts = all.flatMap(_.entries).map(a => schedule.entries.find(s => collide(a, s)).map(f => (f, f.group.members.intersect(a.group.members)))).foldLeft(List.empty[Conflict]) {
      case (list, Some((ee, m))) if m.nonEmpty =>
        val c = Conflict(ee, m.toVector, ee.group)
        list :+ c
      case (list, _) => list
    }

    //check integrity of group-appointment relation
    val integrity = schedule.entries.groupBy(_.group) forall {
      case (_, ss) => ss.size == appointments
    }

    val factor = if (integrity) 0 else 1000

    Evaluation(conflicts.foldRight(factor)(_.member.size + _) * conflicts.size, conflicts)
  }
}