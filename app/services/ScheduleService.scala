package services

import java.util.UUID
import models.{AssignmentPlan, Group}
import models.schedule._
import org.joda.time.{LocalDate, LocalTime}
import utils.{Genesis, Gen}
import utils.TypeClasses._
import scala.language.higherKinds
import scala.util.Random._
import scalaz.Functor
import services.ScheduleService._
import utils.Evaluation
import TimetableDateEntry._
import utils.Ops.FunctorInstances._
import utils.Ops.MonoidInstances.intM

// TODO: refactor out of file
case class Conflict(entry: ScheduleEntryG, members: Vector[UUID], group: Group)

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
  def population(times: Int, timetable: Timetable, assignmentPlan: AssignmentPlan, groups: Set[Group]): Vector[ScheduleG]
  def mutate: Mutation
  def mutateDestructive: Mutation
  def crossover: Crossover
  def crossoverDestructive: Crossover
  def evaluation(all: Vector[ScheduleG], appointments: Int): Evaluator
}

trait ScheduleGenesisServiceLike {
  def generate(timetable: Timetable, groups: Set[Group], assignmentPlan: AssignmentPlan, competitive: Vector[ScheduleG]): (Gen[ScheduleG, Conflict, Int], Int)
}

object ScheduleService {
  type Mutation = Mutate[ScheduleG, Conflict, Int]
  type Crossover = Cross[ScheduleG, Conflict, Int]
  type Evaluator = Eval[ScheduleG, Conflict, Int]
  type Evaluation = utils.Evaluation[Conflict, Int]

  def mutation(f: (ScheduleG, Evaluation) => ScheduleG) = Mutate.instance[ScheduleG, Conflict, Int](f)

  def cross(f: ((ScheduleG, Evaluation), (ScheduleG, Evaluation)) => (ScheduleG, ScheduleG)) = Cross.instance[ScheduleG, Conflict, Int](f)

  def eval(f: ScheduleG => Evaluation)= Eval.instance[ScheduleG, Conflict, Int](f)

  def swap[A, F[X]](f: F[A])(left: A, right: A)(implicit F: Functor[F]): F[A] = F.map(f) {
    case x if x == left => right
    case y if y == right => left
    case z => z
  }

  def randomOne[A](v: Vector[A]): A = shuffle(v).head

  @annotation.tailrec
  def randomAvoiding(avoiding: Group)(implicit groups: Vector[Group]): Group = {
    val grp = randomGroup
    if(grp.id == avoiding.id) randomAvoiding(avoiding)
    else grp
  }

  def randomGroup(implicit groups: Vector[Group]): Group = groups(nextInt(groups.size))

  def replaceGroup(s: Group)(f: Set[UUID] => Set[UUID]): Group = Group(s.label, s.labwork, f(s.members), s.id)

  def replaceSchedule(s: ScheduleG)(f: ScheduleEntryG => ScheduleEntryG): ScheduleG = ScheduleG(s.labwork, s.entries map f, s.id)

  def replaceEntry(e: ScheduleEntryG)(f: Group => Group) = ScheduleEntryG(e.start, e.end, e.date, e.room, e.supervisor, f(e.group), e.id)

  def replaceWithin(s: ScheduleG)(left: Group, right: Group): ScheduleG = replaceSchedule(s)(
    replaceEntry(_) {
      case x if x.id == left.id => right
      case y if y.id == right.id => left
      case z => z
    })

  def collide(left: ScheduleEntryG, right: ScheduleEntryG): Boolean = {
    left.date.isEqual(right.date) && left.start.isEqual(right.start) || (right.start.isAfter(left.start) && right.start.isBefore(left.end))
  }

  def exchange(left: UUID, right: UUID, s: ScheduleG) = replaceSchedule(s)(replaceEntry(_)(replaceGroup(_)(swap(_)(left, right))))
}

class ScheduleService(private val timetableService: TimetableServiceLike) extends ScheduleServiceLike with ScheduleGenesisServiceLike { self =>

  override def generate(timetable: Timetable, groups: Set[Group], assignmentPlan: AssignmentPlan, competitive: Vector[ScheduleG]): (Gen[ScheduleG, Conflict, Int], Int) = {
    val pop = population(100, timetable, assignmentPlan, groups)

    implicit val evalF = evaluation(competitive, assignmentPlan.numberOfEntries)
    implicit val mutateF = (mutate, mutateDestructive)
    implicit val crossF = (crossover, crossoverDestructive)
    import utils.TypeClasses.instances._

    Genesis.measureByVariation[ScheduleG, Conflict, Int](pop, 200) { elite =>
      if (elite.size % 10 == 0) elite.take(10).distinct.size == 1 else false
    }
  }

  override def population(times: Int, timetable: Timetable, assignmentPlan: AssignmentPlan, groups: Set[Group]): Vector[ScheduleG] = {
    val entries = timetableService.extrapolateEntries(timetable, assignmentPlan, groups)
    (0 until times).map(_ => populate(timetable, entries, assignmentPlan, groups)).toVector
  }

  private def populate(timetable: Timetable, entries: Set[TimetableDateEntry], assignmentPlan: AssignmentPlan, groups: Set[Group]): ScheduleG = {
    val sg = shuffle(groups.toVector)
    val scheduleEntries = entries.toVector.sortBy(toLocalDateTime).grouped(groups.size).toVector.flatMap(_.zip(sg).map {
      case (t, group) =>
        val o = TimetableDateEntry.organizer(t, timetable.entries)
        ScheduleEntryG(t.start, t.end, t.date, o.room, o.supervisor, group, ScheduleEntry.randomUUID)
    }).toSet

    ScheduleG(timetable.labwork, scheduleEntries, Schedule.randomUUID)
  }

  override def mutate: Mutation = mutation { (s, e) =>
    implicit val groups = s.entries.toVector.map(_.group)
    val group1 = randomGroup
    val group2 = randomAvoiding(group1)
    replaceWithin(s)(group1, group2)
  }

  override def mutateDestructive: Mutation = mutation { (s, e) =>
    implicit val groups = s.entries.map(_.group).toVector
    e.fold {
      case ((h :: t, _)) =>
        val group = randomAvoiding(h.group)
        val chosenOne = randomOne(h.members)
        val swappedOne = randomOne(group.members.toVector)
        val (cgroup, sgroup) = (
          replaceGroup(h.group)(swap(_)(chosenOne, swappedOne)),
          replaceGroup(group)(swap(_)(swappedOne, chosenOne))
          )
        replaceWithin(s)(cgroup, sgroup)
      case ((Nil, _)) => s
    }
  }

  override def crossover: Crossover = cross {
    case ((s1, e1), (s2, e2)) =>
      (shuffle(e1.err), shuffle(e2.err)) match {
        case (h1 :: _, h2 :: _) =>
          lazy val rl = replaceWithin(s1)(h1.group, randomAvoiding(h1.group)(s1.entries.map (_.group).toVector))
          lazy val rr = replaceWithin(s2)(h2.group, randomAvoiding(h2.group)(s2.entries.map (_.group).toVector))
          (rl, rr)
        case _ => (s1, s2)
      }
  }

  override def crossoverDestructive: Crossover = cross {
    case ((s1, e1), (s2, e2)) =>
      def newOne(ev: utils.Evaluation[Conflict, Int], left: ScheduleG, right: ScheduleG): ScheduleG = ev.fold {
        case ((c :: t), _) =>
          val one = randomOne(c.members)
          right.entries.find(e => !e.group.members.contains(one)) match {
            case Some(e) => exchange(one, e.group.members.head, left)
            case None =>
              val ex = randomGroup(right.entries.map (_.group).toVector).members.head
              exchange(one, ex, left)
          }
        case ((Nil, _)) => left
      }

      (newOne(e1, s1, s2), newOne(e2, s2, s1))
  }

  override def evaluation(all: Vector[ScheduleG], appointments: Int): Evaluator = eval { schedule =>
    val factor = {
      val integrity = schedule.entries.groupBy(_.group).forall(t => t._2.size == appointments)
      if (integrity) 0 else 1000
    }

    val conflicts = for {
      globalEntry <- all flatMap (_.entries)
      entries = schedule.entries
      collision = entries find (collide(globalEntry, _))
      intersection = collision map (_.group.members intersect globalEntry.group.members)
    } yield for {
      entry <- collision
      members <- intersection if members.nonEmpty
    } yield Conflict(entry, members.toVector, entry.group)

    conflicts.foldLeft(Evaluation.empty[Conflict, Int]) {
      case (eval, Some(c)) => eval add c map (_ + c.members.size + factor)
      case (eval, _) => eval
    }
  }
//  override def evaluate(schedule: ScheduleG, appointments: Int, all: Vector[ScheduleG]): Evaluation = {
//    def collide(left: ScheduleEntryG, right: ScheduleEntryG): Boolean = {
//      left.date.isEqual(right.date) && left.start.isEqual(right.start) || (right.start.isAfter(left.start) && right.start.isBefore(left.end))
//    }
//
//    // /check entries against each already existing schedule
//    val conflicts = all.flatMap(_.entries).map(a => schedule.entries.find(s => collide(a, s)).map(f => (f, f.group.members.intersect(a.group.members)))).foldLeft(List.empty[Conflict]) {
//      case (list, Some((ee, m))) if m.nonEmpty =>
//        val c = Conflict(ee, m.toVector, ee.group)
//        list :+ c
//      case (list, _) => list
//    }
//
//    //check integrity of group-appointment relation
//    val integrity = schedule.entries.groupBy(_.group) forall {
//      case (_, ss) => ss.size == appointments
//    }
//
//    val factor = if (integrity) 0 else 1000
//
//    Evaluation(conflicts.foldRight(factor)(_.members.size + _) * conflicts.size, conflicts)
//  }
}