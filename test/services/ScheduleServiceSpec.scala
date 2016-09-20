package services

import java.util.UUID

import base.TestBaseDefinition
import models.labwork._
import models.semester.Semester
import models.users.User
import models._
import org.joda.time.{DateTime, LocalDate, LocalTime, Weeks}
import org.joda.time.format.DateTimeFormat
import org.scalatest.WordSpec
import utils.{Evaluation, Gen}

import scala.language.postfixOps
import scala.util.Random._
import utils.Ops.MonoidInstances._

object ScheduleServiceSpec {
  def emptyEval: Evaluation[Conflict, Int] = Evaluation.empty[Conflict, Int]
  def eval(l: List[Conflict]): Evaluation[Conflict, Int] = Evaluation.withError[Conflict, Int](l)

  def unfold[A, B](a: A)(f: A => Option[(B, A)]): Stream[B] = f(a) match {
    case Some((b, aa)) => Stream.cons(b, unfold(aa)(f))
    case None => Stream.empty
  }

  def alph(amount: Int): Vector[String] = {
    unfold('A')(a => Option((a.toString, (a + 1).toChar))) take (amount % 27) toVector
  }

  def assignmentPlan(amount: Int, duration: Int = 1): AssignmentPlan = {
    val entries = (0 until amount).map(n => AssignmentEntry(n, "label", Set.empty, duration)).toSet
    AssignmentPlan(UUID.randomUUID(), amount, amount, entries)
  }

  def population(n: Int): Vector[UUID] = Stream.continually(UUID.randomUUID()) take n toVector
}

class ScheduleServiceSpec extends WordSpec with TestBaseDefinition {
  import services.ScheduleServiceSpec._
  
  val blacklistService = new BlacklistService
  val timetableService = new TimetableService(blacklistService)
  val scheduleService = new ScheduleService(timetableService)

  val semester = Semester("", "", LocalDate.now, LocalDate.now.plusWeeks(30), LocalDate.now.plusWeeks(4))
  val weeks = Weeks.weeksBetween(semester.start, semester.examStart)

  val ft = DateTimeFormat.forPattern("HH:mm:ss")
  val fd = DateTimeFormat.forPattern("dd/MM/yyyy")

  def gen(specs: Vector[(Timetable, Set[Group], AssignmentPlan)]): Vector[(Gen[ScheduleG, Conflict, Int], Int)] = {

    specs.foldLeft((Vector.empty[ScheduleG], Vector.empty[(Gen[ScheduleG, Conflict, Int], Int)])) {
      case ((comp, _), (t, g, ap)) =>
        val result = scheduleService.generate(t, g, ap, semester, comp)

        (comp ++ Vector(result._1.elem), Vector((result._1, result._2)))
    }._2
  }

  "A ScheduleService" should {

    import TimetableDateEntry._

    "populate initial schedules any times" in {
      val entries = (0 until 6).map(n => TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(n).index, LocalTime.now, LocalTime.now)).toSet
      val timetable = Timetable(Labwork.randomUUID, entries, LocalDate.now, Set.empty[DateTime])
      val plan = assignmentPlan(5)
      val groups = alph(8).map(a => Group(a, UUID.randomUUID(), Set.empty)).toSet

      val times = 100
      val extrapolated = timetableService.extrapolateTimetableByWeeks(timetable, weeks, plan, groups)
      val result = scheduleService.population(times, timetable.labwork, extrapolated, groups)
      //println(result.head.entries.toVector.sortBy(toLocalDateTime).map(_.group.label).take(groups.size))
      //println(result.last.entries.toVector.sortBy(toLocalDateTime).map(_.group.label).take(groups.size))

      result.nonEmpty shouldBe true
      result.size shouldBe times

      result.foldLeft((true, result.head)) {
        case ((b, p), n) =>
          val prev = p.entries.sortBy(toLocalDateTime).map(_.group.label).take(groups.size)
          val next = n.entries.sortBy(toLocalDateTime).map(_.group.label).take(groups.size)
          (b && prev == next, n)
      }._1 shouldBe false

      result.foreach(f => f.entries.size shouldBe plan.entries.size * groups.size)
      groups.forall { group =>
        result.forall(_.entries.count(_.group.id == group.id) == plan.entries.size)
      } shouldBe true

      val g = result.map(_.entries.sortBy(toLocalDateTime).map(_.group.label).grouped(groups.size).toVector)
      val gv = groups.map(_.id)
      g.forall(vec => vec.foldLeft((true, vec.head)) {
        case ((b, rep), l) => (b && l == rep, rep)
      }._1) shouldBe true
      result.forall(_.entries.sortBy(toLocalDateTime).map(_.group.id).grouped(groups.size).toVector.forall(v1 => v1.forall(gv.contains) && v1.size == gv.size)) shouldBe true
    }

    "mutate given schedule by swapping two randomly chosen groups" in {
      import scala.util.Random._

      val plan = assignmentPlan(8)
      val groups = alph(8).map(Group(_, UUID.randomUUID(), Set.empty))
      val entries = (0 until plan.entries.size * groups.size).grouped(groups.size).flatMap(_.zip(groups)).map {
        case (n, group) =>
        val date = LocalDate.now.plusWeeks(n)
        val start = LocalTime.now.withHourOfDay(nextInt(19))
        val end = start.plusHours(nextInt(3))

        ScheduleEntryG(start, end, date, Room.randomUUID, Set(User.randomUUID), group)
      }.toVector
      val schedule = ScheduleG(UUID.randomUUID(), entries, UUID.randomUUID())

      val result = (0 until 100).map(_ => scheduleService.mutate(schedule, emptyEval)).toVector

      /*schedule.entries.toVector.sortBy(toLocalDateTime).map(_.group.label).grouped(groups.size).foreach(println)
      println("=========================")
      result.foreach(_.entries.toVector.sortBy(toLocalDateTime).map(_.group.label).grouped(groups.size).toVector.foreach(println))*/

      result.foreach(_.entries.sortBy(toLocalDateTime) should not be schedule.entries.sortBy(toLocalDateTime))
      val g = result.map(_.entries.sortBy(toLocalDateTime).map(_.group.label).grouped(groups.size).toVector)
      val gv = groups.map(_.id)
      g.forall(vec => vec.foldLeft((true, vec.head)) {
        case ((b, rep), l) => (b && l == rep, rep)
      }._1) shouldBe true
      result.forall(_.entries.sortBy(toLocalDateTime).map(_.group.id).grouped(groups.size).toVector.forall(v1 => v1.forall(gv.contains) && v1.size == gv.size)) shouldBe true
    }

    "successfully cross two schedules" in {
      import scala.util.Random._

      val plan = assignmentPlan(8)
      val groups = alph(8).map(Group(_, UUID.randomUUID(), Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID)))
      val g1 = shuffle(groups)
      val g2 = shuffle(groups)

      val entries = {
        (0 until plan.entries.size * groups.size).grouped(groups.size).flatMap(_.zip(g1).zip(g2).map {
          case (n, group) =>
            val date = LocalDate.now.plusWeeks(n._1)
            val start = LocalTime.now.withHourOfDay(nextInt(19))
            val end = start.plusHours(nextInt(3))

            (ScheduleEntryG(start, end, date, Room.randomUUID, Set(User.randomUUID), n._2), ScheduleEntryG(start, end, date, Room.randomUUID, Set(User.randomUUID), group))
        }).toVector
      }.unzip

      val left = ScheduleG(UUID.randomUUID(), entries._1, UUID.randomUUID())
      val right = ScheduleG(UUID.randomUUID(), entries._2, UUID.randomUUID())
      val lgroup = left.entries.head.group
      val rgroup = right.entries.head.group
      val eLeft = eval(List(Conflict(left.entries.head, left.entries.head.group.members.toVector.take(3), lgroup)))
      val eRight = eval(List(Conflict(right.entries.head, right.entries.head.group.members.toVector.take(3), rgroup)))

      val result = scheduleService.crossover((left, eLeft), (right, eRight))

      left should not be result._1
      right should not be result._2

      result._1.entries.find(_.group == lgroup).forall(a => left.entries.find(_.group == a.group).contains(a)) shouldBe false
      result._2.entries.find(_.group == rgroup).forall(a => right.entries.find(_.group == a.group).contains(a)) shouldBe false

      result._1.entries.zip(left.entries).count(e => e._1 == e._2) < left.entries.size shouldBe true
      result._2.entries.zip(right.entries).count(e => e._1 == e._2) < right.entries.size shouldBe true

      val leftCount = result._1.entries.count(c => left.entries.contains(c))
      leftCount != left.entries.size && leftCount < left.entries.size && leftCount > 0 && leftCount > left.entries.size / 2 shouldBe true

      val rightCount = result._2.entries.count(c => right.entries.contains(c))
      rightCount != right.entries.size && rightCount < right.entries.size && rightCount > 0 && rightCount > right.entries.size / 2 shouldBe true

      Vector(result._1, result._2).foreach { s =>
        val gg = s.entries.sortBy(toLocalDateTime).map(_.group.label).grouped(groups.size).toVector
        val gv = groups.map(_.id)
        gg.foldLeft((true, gg.head)) {
          case ((b, rep), l) => (b && l == rep, rep)
        }._1 shouldBe true
        s.entries.sortBy(toLocalDateTime).map(_.group.id).grouped(groups.size).toVector.forall(v1 => v1.forall(gv.contains) && v1.size == gv.size) shouldBe true
      }
    }

    "evaluate a given schedule when there are no other schedules" in {
      val plan = assignmentPlan(5)
      val labwork = Labwork("label", "description", Semester.randomUUID, Course.randomUUID, Degree.randomUUID)
      val entries = (0 until 6).map(n => TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(n).index, LocalTime.now, LocalTime.now)).toSet
      val timetable = Timetable(labwork.id, entries, LocalDate.now, Set.empty[DateTime])

      val groups = Set(
        Group("A", labwork.id, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID)),
        Group("B", labwork.id, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID)),
        Group("C", labwork.id, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID))
      )
      val existing = Vector.empty[ScheduleG]

      val extrapolated = timetableService.extrapolateTimetableByWeeks(timetable, weeks, plan, groups)
      val schedule = scheduleService.population(1, labwork.id, extrapolated, groups).head
      val result = scheduleService.evaluation(existing, plan.entries.size)(schedule)

      result.err shouldBe empty
      result.value shouldBe 0
    }

    "evaluate a given schedule when there are some other schedules" in {
      val plan = assignmentPlan(8)
      val ap1 = Course("ap1", "c1", "abbrev", User.randomUUID, 1)
      val ma1 = Course("ma1", "c2", "abbrev", User.randomUUID, 1)
      val degree = Degree.randomUUID
      val semester1 = Semester("semester1", "abbrev", LocalDate.now, LocalDate.now, LocalDate.now)
      val ap1Prak = Labwork("ap1Prak", "desc1", semester1.id, ap1.id, degree)
      val ma1Prak = Labwork("ma1Prak", "desc2", semester1.id, ma1.id, degree)

      val ap1Entries = Set(
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("27/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("09:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("27/10/2015")).index, ft.parseLocalTime("09:00:00"), ft.parseLocalTime("10:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("27/10/2015")).index, ft.parseLocalTime("10:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("14:00:00"), ft.parseLocalTime("15:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("14:00:00"), ft.parseLocalTime("15:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("15:00:00"), ft.parseLocalTime("16:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("15:00:00"), ft.parseLocalTime("16:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("16:00:00"), ft.parseLocalTime("17:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("16:00:00"), ft.parseLocalTime("17:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("09:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("09:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("09:00:00"), ft.parseLocalTime("10:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("09:00:00"), ft.parseLocalTime("10:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("10:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("10:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("12:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("12:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("12:00:00"), ft.parseLocalTime("13:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("12:00:00"), ft.parseLocalTime("13:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("13:00:00"), ft.parseLocalTime("14:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("13:00:00"), ft.parseLocalTime("14:00:00"))
      )
      val ma1Entries = Set(
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("26/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("27/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("14:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("14:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("14:00:00"), ft.parseLocalTime("17:00:00"))
      )

      val students = (0 until 300).map(_ => User.randomUUID).toVector
      val ap1G = shuffle(students).take(250).grouped(10).map(s => Group("", ap1Prak.id, s.toSet)).toSet
      val ma1G = shuffle(students).take(240).grouped(20).map(s => Group("", ma1Prak.id, s.toSet)).toSet

      val ap1T = Timetable(ap1Prak.id, ap1Entries, fd.parseLocalDate("27/10/2015"), Set.empty[DateTime])
      val ma1T = Timetable(ma1Prak.id, ma1Entries, fd.parseLocalDate("26/10/2015"), Set.empty[DateTime])

      val extrapolatedAp1 = timetableService.extrapolateTimetableByWeeks(ap1T, weeks, plan, ap1G)
      val extrapolatedMa1 = timetableService.extrapolateTimetableByWeeks(ma1T, weeks, plan, ma1G)
      val ap1Schedule = scheduleService.population(1, ap1Prak.id, extrapolatedAp1, ap1G)
      val ma1Schedule = scheduleService.population(1, ma1Prak.id, extrapolatedMa1, ma1G).head

      val result = scheduleService.evaluation(ap1Schedule, plan.entries.size)(ma1Schedule)
//      println(s"conflicts ${result.err.size}")
//      println(s"guys ${result.err.map(_.members)}")
//      println(s"date ${result.err.map(e => e.entry.date.toLocalDateTime(e.entry.start))}")
//      println(s"value ${result.value}")

      result.err should not be empty
      result.value should be < 1000
      result.err.size should be <= result.value
      result.err.forall(c => ma1G.contains(c.group) && c.members.forall(u => ma1G.exists(_.members.contains(u))) && ma1Schedule.entries.contains(c.entry)) shouldBe true
      result.err.forall(c => ap1G.contains(c.group) && c.members.forall(u => ap1G.exists(_.members.contains(u))) && ap1Schedule.head.entries.contains(c.entry)) shouldBe false
    }

    "mutate given schedule destructively by exchanging people between groups" in {
      val labid = UUID.randomUUID()
      val groups = alph(10) zip (population(100) grouped 10 toVector) map {
        case (label, group) => Group(label, labid, group toSet)
      }
      val entries = groups map (ScheduleEntryG(LocalTime.now, LocalTime.now, LocalDate.now, Room.randomUUID, Set(User.randomUUID), _))
      val schedule = ScheduleG(labid, entries, Schedule.randomUUID)
      val ev = eval(List(Conflict(entries(4), entries(4).group.members take 2 toVector, entries(4).group)))

      val newSchedule = scheduleService.mutateDestructive(schedule, ev)

      val theGroup = entries(4).group
      val theNaughtyOnes = theGroup.members take 2 toVector

      newSchedule.entries find (_.group.id == theGroup.id) match {
        case Some(entry) =>
          //should not contain one or the other
          entry.group.members.contains(theNaughtyOnes.head) && entry.group.members.contains(theNaughtyOnes(1)) shouldBe false
          //should have one new member
          (entry.group.members diff (theGroup members)).size shouldBe 1

        case None => fail("Groups should not simply disappear")
      }

      //The swapped individual should find himself in the sacrifice's former group and vice versa
      (for {
        groupThatAcceptedConflict <- newSchedule.entries.map(_.group) find (z => ((z.members intersect theGroup.members).size == 1) && z.id != theGroup.id)
        groupThatAcceptedConflictBefore <- schedule.entries find (_.group.id == groupThatAcceptedConflict.id)
        newGroupOfSacrifice <- newSchedule.entries find (_.group.id == theGroup.id)
      } yield {
        val sacrifice = newGroupOfSacrifice.group.members.diff(theGroup members).head
        groupThatAcceptedConflictBefore.group.members contains sacrifice
      }) match {
        case Some(b) => b shouldBe true
        case None => fail("The old group should contain the one that was selected as a swapping sacrifice")
      }
      schedule.labwork shouldBe newSchedule.labwork
    }

    "not change the size of the groups or create duplicates after a destructive mutation" in {
      val labid = UUID.randomUUID()
      val groups = alph(10) zip (population(100) grouped 10 toVector) map {
        case (label, group) => Group(label, labid, group toSet)
      }
      val entries = groups map (ScheduleEntryG(LocalTime.now, LocalTime.now, LocalDate.now, Room.randomUUID, Set(User.randomUUID), _))
      val schedule = ScheduleG(labid, entries, Schedule.randomUUID)
      val ev = eval(List(Conflict(entries(4), entries(4).group.members take 2 toVector, entries(4).group)))

      val newSchedule = scheduleService.mutateDestructive(schedule, ev)

      schedule.entries map (_ group) sortBy (_.label) zip (newSchedule.entries map (_ group) sortBy (_ label)) foreach {
        case ((pgroup, cgroup)) =>
          pgroup.members.size shouldBe cgroup.members.size
      }
    }

    "cross conflicting people destructively with others from different schedules" in {
      val labid = UUID.randomUUID()
      val groups = alph(10) zip (population(100) grouped 10 toVector) map {
        case (label, group) => Group(label, labid, group toSet)
      }
      def entries = shuffle(groups) map (ScheduleEntryG(LocalTime.now, LocalTime.now, LocalDate.now, Room.randomUUID, Set(User.randomUUID), _))
      def schedule = ScheduleG(labid, entries, Schedule.randomUUID)

      val (schedule1, schedule2) = (schedule, schedule)
      val (e1, e2) = (schedule1.entries.toVector(3), schedule2.entries.toVector(5))

      val (eval1, eval2) = (Evaluation(List(Conflict(e1, e1.group.members take 2 toVector, e1.group)), 0),
        Evaluation(List(Conflict(e2, e2.group.members take 2 toVector, e2.group)), 0))


      val (s1, s2) = scheduleService.crossoverDestructive((schedule1, eval1), (schedule2, eval2))

      s1.entries find (_.group.id == e1.group.id) match {
        case Some(entry) =>
          val members = (e1.group.members take 2).toVector
          //should not contain one or the other
          entry.group.members.contains(members.head) && entry.group.members.contains(members(1)) shouldBe false
          //should have one new member
          (entry.group.members diff e1.group.members).size shouldBe 1

        case None => fail("Groups should not simply disappear")
      }

      s2.entries find (_.group.id == e2.group.id) match {
        case Some(entry) =>
          val members = (e2.group.members take 2).toVector
          //should not contain one or the other
          entry.group.members.contains(members.head) && entry.group.members.contains(members(1)) shouldBe false
          //should have one new member
          (entry.group.members diff e2.group.members).size shouldBe 1

        case None => fail("Groups should not simply disappear")
      }

      schedule1.labwork shouldBe s1.labwork
      schedule2.labwork shouldBe s2.labwork
    }

    "not change the size of the groups or create duplicates after a destructive crossover" in {
      val labid = UUID.randomUUID()
      val groups = alph(10) zip (population(100) grouped 10 toVector) map {
        case (label, group) => Group(label, labid, group toSet)
      }
      def entries = shuffle(groups) map (ScheduleEntryG(LocalTime.now, LocalTime.now, LocalDate.now, Room.randomUUID, Set(User.randomUUID), _))
      def schedule = ScheduleG(labid, entries, Schedule.randomUUID)

      val (schedule1, schedule2) = (schedule, schedule)
      val (e1, e2) = (schedule1.entries(3), schedule2.entries
      (5))

      val (eval1, eval2) = (Evaluation(List(Conflict(e1, e1.group.members take 2 toVector, e1.group)), 0),
        Evaluation(List(Conflict(e2, e2.group.members take 2 toVector, e2.group)), 0))


      val (s1, s2) = scheduleService.crossoverDestructive((schedule1, eval1), (schedule2, eval2))

      schedule1.entries map (_ group) sortBy (_ label) zip (s1.entries map (_ group) sortBy (_ label)) foreach {
        case ((pgroup, cgroup)) =>
          pgroup.members.size shouldBe cgroup.members.size
      }

      schedule2.entries map (_ group) sortBy (_ label) zip (s2.entries map (_ group) sortBy (_ label)) foreach {
        case ((pgroup, cgroup)) =>
          pgroup.members.size shouldBe cgroup.members.size
      }
    }
  }

  "A ScheduleGenesisService" should {

    "generate an initial collision free schedule instantly" in {
      val ap1Plan = assignmentPlan(8)
      val ap1 = Course("ap1", "c1", "abbrev", User.randomUUID, 1)
      val semester1 = Semester("semester1", "abbrev", LocalDate.now, LocalDate.now, LocalDate.now)
      val ap1Prak = Labwork("ap1Prak", "desc1", semester1.id, ap1.id, Degree.randomUUID)

      val ap1Entries = Set(
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("27/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("09:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("27/10/2015")).index, ft.parseLocalTime("09:00:00"), ft.parseLocalTime("10:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("27/10/2015")).index, ft.parseLocalTime("10:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("14:00:00"), ft.parseLocalTime("15:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("14:00:00"), ft.parseLocalTime("15:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("15:00:00"), ft.parseLocalTime("16:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("15:00:00"), ft.parseLocalTime("16:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("16:00:00"), ft.parseLocalTime("17:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("16:00:00"), ft.parseLocalTime("17:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("09:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("09:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("09:00:00"), ft.parseLocalTime("10:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("09:00:00"), ft.parseLocalTime("10:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("10:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("10:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("12:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("12:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("12:00:00"), ft.parseLocalTime("13:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("12:00:00"), ft.parseLocalTime("13:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("13:00:00"), ft.parseLocalTime("14:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("13:00:00"), ft.parseLocalTime("14:00:00"))
      )

      import scala.util.Random._
      val students = (0 until 200).map(_ => User.randomUUID).toVector
      val ap1G = shuffle(students).take(200).grouped(10).map(s => Group("", ap1Prak.id, s.toSet)).toSet

      val ap1T = Timetable(ap1Prak.id, ap1Entries, fd.parseLocalDate("27/10/2015"), Set.empty[DateTime])

      val result = gen(Vector(
        (ap1T, ap1G, ap1Plan)
      )).head

//      println(s"gen ${result._2}")
//      println(s"conflict size ${result._1.evaluate.err.size}")
//      println(s"conflict value ${result._1.evaluate.value}")
      result._1.evaluate.err shouldBe empty
      result._1.evaluate.value shouldBe 0

      result._2 shouldBe 0
      result._1.elem.labwork shouldEqual ap1Prak.id
      result._1.elem.entries.groupBy(_.group) forall {
        case (_, ss) => ss.size == ap1Plan.entries.size
      } shouldBe true
    }

    "generate a schedule with minimal or no collisions considering one existing competitive schedule and more density" in {
      val ap1Plan = assignmentPlan(8)
      val ma1Plan = assignmentPlan(4, 2)
      val ap1 = Course("ap1", "c1", "abbrev", User.randomUUID, 1)
      val ma1 = Course("ma1", "c2", "abbrev", User.randomUUID, 1)
      val degree = Degree.randomUUID
      val semester1 = Semester("semester1", "abbrev", LocalDate.now, LocalDate.now, LocalDate.now)
      val ap1Prak = Labwork("ap1Prak", "desc1", semester1.id, ap1.id, degree)
      val ma1Prak = Labwork("ma1Prak", "desc2", semester1.id, ma1.id, degree)

      val ap1Entries = Set(
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("27/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("09:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("27/10/2015")).index, ft.parseLocalTime("09:00:00"), ft.parseLocalTime("10:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("27/10/2015")).index, ft.parseLocalTime("10:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("14:00:00"), ft.parseLocalTime("15:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("14:00:00"), ft.parseLocalTime("15:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("15:00:00"), ft.parseLocalTime("16:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("15:00:00"), ft.parseLocalTime("16:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("16:00:00"), ft.parseLocalTime("17:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("16:00:00"), ft.parseLocalTime("17:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("09:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("09:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("09:00:00"), ft.parseLocalTime("10:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("09:00:00"), ft.parseLocalTime("10:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("10:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("10:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("12:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("12:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("12:00:00"), ft.parseLocalTime("13:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("12:00:00"), ft.parseLocalTime("13:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("13:00:00"), ft.parseLocalTime("14:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("13:00:00"), ft.parseLocalTime("14:00:00"))
      )
      val ma1Entries = Set(
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("26/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("27/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("14:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("14:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("14:00:00"), ft.parseLocalTime("17:00:00"))
      )

      val students = (0 until 200).map(_ => User.randomUUID).toVector
      val ap1G = shuffle(students).take(180).grouped(10).map(s => Group("", ap1Prak.id, s.toSet)).toSet
      val ma1G = shuffle(students).take(180).grouped(20).map(s => Group("", ma1Prak.id, s.toSet)).toSet

      val ap1T = Timetable(ap1Prak.id, ap1Entries, fd.parseLocalDate("27/10/2015"), Set.empty[DateTime])
      val ma1T = Timetable(ma1Prak.id, ma1Entries, fd.parseLocalDate("26/10/2015"), Set.empty[DateTime])

      val result = gen(Vector(
        (ap1T, ap1G, ap1Plan),
        (ma1T, ma1G, ma1Plan)
      )).head

//      println(s"gen ${result._2}")
//      println(s"conflict size ${result._1.evaluate.err.size}")
//      println(s"conflict value ${result._1.evaluate.value}")
      result._1.evaluate.err shouldBe empty
      result._1.evaluate.value shouldBe 0

      result._2 should be >= 0
      result._1.elem.labwork shouldEqual ma1Prak.id
      result._1.elem.entries.groupBy(_.group) forall {
        case (_, ss) => ss.size == ma1Plan.entries.size
      } shouldBe true
    }


    "generate a schedule with minimal or no conflicts considering two existing competitive schedules and more density" in {
      val ap1Plan = assignmentPlan(8)
      val ma1Plan = assignmentPlan(4, 2)
      val gdvkPlan = assignmentPlan(4)
      val ap1 = Course("ap1", "c1", "abbrev", User.randomUUID, 1)
      val ma1 = Course("ma1", "c2", "abbrev", User.randomUUID, 1)
      val gdvk = Course("gdvk", "c3", "abbrev", User.randomUUID, 1)
      val degree = Degree.randomUUID
      val semester1 = Semester("semester1", "abbrev", LocalDate.now, LocalDate.now, LocalDate.now)
      val ap1Prak = Labwork("ap1Prak", "desc1", semester1.id, ap1.id, degree)
      val ma1Prak = Labwork("ma1Prak", "desc2", semester1.id, ma1.id, degree)
      val gdvkPrak = Labwork("gdvkPrak", "desc3", semester1.id, gdvk.id, degree)

      val ap1Entries = Set(
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("27/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("09:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("27/10/2015")).index, ft.parseLocalTime("09:00:00"), ft.parseLocalTime("10:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("27/10/2015")).index, ft.parseLocalTime("10:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("14:00:00"), ft.parseLocalTime("15:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("14:00:00"), ft.parseLocalTime("15:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("15:00:00"), ft.parseLocalTime("16:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("15:00:00"), ft.parseLocalTime("16:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("16:00:00"), ft.parseLocalTime("17:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("16:00:00"), ft.parseLocalTime("17:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("09:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("09:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("09:00:00"), ft.parseLocalTime("10:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("09:00:00"), ft.parseLocalTime("10:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("10:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("10:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("12:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("12:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("12:00:00"), ft.parseLocalTime("13:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("12:00:00"), ft.parseLocalTime("13:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("13:00:00"), ft.parseLocalTime("14:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("13:00:00"), ft.parseLocalTime("14:00:00"))
      )
      val ma1Entries = Set(
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("26/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("27/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("14:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("14:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("14:00:00"), ft.parseLocalTime("17:00:00"))
      )
      val gdvkEntries = Set(
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("13:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("14:00:00"), ft.parseLocalTime("17:00:00"))
      )

      val students = (0 until 200).map(_ => User.randomUUID).toVector
      val ap1G = shuffle(students).take(180).grouped(10).map(s => Group("", ap1Prak.id, s.toSet)).toSet
      val ma1G = shuffle(students).take(180).grouped(20).map(s => Group("", ma1Prak.id, s.toSet)).toSet
      val gdvkG = shuffle(students).take(150).grouped(30).map(s => Group("", gdvkPrak.id, s.toSet)).toSet

      val ap1T = Timetable(ap1Prak.id, ap1Entries, fd.parseLocalDate("27/10/2015"), Set.empty[DateTime])
      val ma1T = Timetable(ma1Prak.id, ma1Entries, fd.parseLocalDate("26/10/2015"), Set.empty[DateTime])
      val gdvkT = Timetable(gdvkPrak.id, gdvkEntries, fd.parseLocalDate("30/10/2015"), Set.empty[DateTime])

      val result = gen(Vector(
        (ap1T, ap1G, ap1Plan),
        (ma1T, ma1G, ma1Plan),
        (gdvkT, gdvkG, gdvkPlan)
      )).head

//      println(s"gen ${result._2}")
//      println(s"conflict size ${result._1.evaluate.err.size}")
//      println(s"conflict value ${result._1.evaluate.value}")
      result._1.evaluate.err.size <= 2 shouldBe true

      result._2 should be > 0
      result._1.evaluate.err.size <= 2 shouldBe true
      result._1.elem.labwork shouldEqual gdvkPrak.id
      result._1.elem.entries.groupBy(_.group) forall {
        case (_, ss) => ss.size == gdvkPlan.entries.size
      } shouldBe true
    }

    /*"generate a schedules with minimal or no conflicts considering three existing competitive schedules and more density" in {
      println("NOTE: This one takes some time..")
      val ap1Plan = AssignmentPlan(8, Set(
        AssignmentEntry(0, Set.empty[EntryType]),
        AssignmentEntry(1, Set.empty[EntryType]),
        AssignmentEntry(2, Set.empty[EntryType]),
        AssignmentEntry(3, Set.empty[EntryType]),
        AssignmentEntry(4, Set.empty[EntryType]),
        AssignmentEntry(5, Set.empty[EntryType]),
        AssignmentEntry(6, Set.empty[EntryType]),
        AssignmentEntry(7, Set.empty[EntryType])
      ))
      val ma1Plan = AssignmentPlan(4, Set(
        AssignmentEntry(0, Set.empty[EntryType], 2),
        AssignmentEntry(1, Set.empty[EntryType], 2),
        AssignmentEntry(2, Set.empty[EntryType], 2),
        AssignmentEntry(3, Set.empty[EntryType], 2)
      ))
      val gdvkPlan = AssignmentPlan(4, Set(
        AssignmentEntry(0, Set.empty[EntryType]),
        AssignmentEntry(1, Set.empty[EntryType]),
        AssignmentEntry(2, Set.empty[EntryType]),
        AssignmentEntry(3, Set.empty[EntryType])
      ))
      val anotherPlan = AssignmentPlan(6, Set(
        AssignmentEntry(0, Set.empty[EntryType]),
        AssignmentEntry(1, Set.empty[EntryType]),
        AssignmentEntry(2, Set.empty[EntryType]),
        AssignmentEntry(3, Set.empty[EntryType]),
        AssignmentEntry(4, Set.empty[EntryType]),
        AssignmentEntry(5, Set.empty[EntryType])
      ))
      val mi = Degree("mi", "abbrev", Degree.randomUUID)
      val ap1 = Course("ap1", "c1", "abbrev", User.randomUUID, 1, Course.randomUUID)
      val ma1 = Course("ma1", "c2", "abbrev", User.randomUUID, 1, Course.randomUUID)
      val gdvk = Course("gdvk", "c3", "abbrev", User.randomUUID, 1, Course.randomUUID)
      val another = Course("another", "c4", "abbrev", User.randomUUID, 1, Course.randomUUID)
      val semester1 = Semester("semester1", "abbrev", LocalDate.now, LocalDate.now, LocalDate.now, Semester.randomUUID)
      val ap1Prak = Labwork("ap1Prak", "desc1", semester1.id, ap1.id, ap1Plan)
      val ma1Prak = Labwork("ma1Prak", "desc2", semester1.id, ma1.id, ma1Plan)
      val gdvkPrak = Labwork("gdvkPrak", "desc3", semester1.id, gdvk.id, gdvkPlan)
      val anotherPrak = Labwork("anotherPrak", "desc4", semester1.id, another.id, anotherPlan)

      val ap1Entries = Set(
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("27/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("09:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("27/10/2015")).index, ft.parseLocalTime("09:00:00"), ft.parseLocalTime("10:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("27/10/2015")).index, ft.parseLocalTime("10:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("14:00:00"), ft.parseLocalTime("15:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("14:00:00"), ft.parseLocalTime("15:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("15:00:00"), ft.parseLocalTime("16:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("15:00:00"), ft.parseLocalTime("16:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("16:00:00"), ft.parseLocalTime("17:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("16:00:00"), ft.parseLocalTime("17:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("09:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("09:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("09:00:00"), ft.parseLocalTime("10:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("09:00:00"), ft.parseLocalTime("10:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("10:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("10:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("12:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("12:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("12:00:00"), ft.parseLocalTime("13:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("12:00:00"), ft.parseLocalTime("13:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("13:00:00"), ft.parseLocalTime("14:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("13:00:00"), ft.parseLocalTime("14:00:00"))
      )
      val ma1Entries = Set(
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("26/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("27/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("14:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("14:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("14:00:00"), ft.parseLocalTime("17:00:00"))
      )
      val gdvkEntries = Set(
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("13:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("14:00:00"), ft.parseLocalTime("17:00:00"))
      )
      val anotherEntries = Set(
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("26/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("13:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("28/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("13:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("28/10/2015")).index, ft.parseLocalTime("13:00:00"), ft.parseLocalTime("15:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("13:00:00")),
        TimetableEntry(Set(User.randomUUID), Room.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("13:00:00"), ft.parseLocalTime("15:00:00"))
      )

      val students = (0 until 300).map(_ => User.randomUUID).toVector
      val ap1G = shuffle(students).take(280).grouped(10).map(s => Group("", ap1Prak.id, s.toSet, Group.randomUUID)).toSet
      val ma1G = shuffle(students).take(280).grouped(20).map(s => Group("", ma1Prak.id, s.toSet, Group.randomUUID)).toSet
      val gdvkG = shuffle(students).take(270).grouped(30).map(s => Group("", gdvkPrak.id, s.toSet, Group.randomUUID)).toSet
      val anotherG = shuffle(students).take(200).grouped(20).map(s => Group("", anotherPrak.id, s.toSet, Group.randomUUID)).toSet


      val ap1T = Timetable(ap1Prak.id, ap1Entries, fd.parseLocalDate("27/10/2015"), Set.empty[DateTime], Timetable.randomUUID)
      val ma1T = Timetable(ma1Prak.id, ma1Entries, fd.parseLocalDate("26/10/2015"), Set.empty[DateTime], Timetable.randomUUID)
      val gdvkT = Timetable(gdvkPrak.id, gdvkEntries, fd.parseLocalDate("30/10/2015"), Set.empty[DateTime], Timetable.randomUUID)
      val anotherT = Timetable(anotherPrak.id, anotherEntries, fd.parseLocalDate("26/10/2015"), Set.empty[DateTime], Timetable.randomUUID)

      val result = gen(Vector(
        (ap1T, ap1G, ap1Plan),
        (ma1T, ma1G, ma1Plan),
        (gdvkT, gdvkG, gdvkPlan),
        (anotherT, anotherG, anotherPlan)
      )).head

//      println(s"gen ${result._2}")
//      println(s"conflict size ${result._1.evaluate.err.size}")
//      println(s"conflict value ${result._1.evaluate.value}")
      result._1.evaluate.err.size <= 2 shouldBe true

      result._2 should be > 0
      result._1.evaluate.err.size <= 2 shouldBe true
      result._1.elem.labwork shouldEqual anotherPrak.id
      result._1.elem.entries.groupBy(_.group) forall {
        case (_, ss) => ss.size == anotherplan.entries.size
      } shouldBe true
    }*/
  }
}
