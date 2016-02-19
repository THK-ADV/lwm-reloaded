package services

import java.util.UUID

import base.TestBaseDefinition
import models.schedule.TimetableDateEntry
import models.schedule._
import models.semester.{Blacklist, Semester}
import models.users.{Employee, Student}
import models._
import org.joda.time.{LocalTime, LocalDate}
import org.joda.time.format.DateTimeFormat
import org.scalatest.WordSpec
import org.scalatest.mock.MockitoSugar.mock
import org.mockito.Matchers._
import org.mockito.Mockito._
import store.SesameRepository
import utils.Gen
import scala.language.postfixOps
import scala.util.Random._
import scala.util.Success

class ScheduleServiceSpec extends WordSpec with TestBaseDefinition {

  def unfold[A, B](a: A)(f: A => Option[(B, A)]): Stream[B] = f(a) match {
    case Some((b, aa)) => Stream.cons(b, unfold(aa)(f))
    case None => Stream.empty
  }

  def alph(amount: Int): Set[String] = {
    unfold('A')(a => Option((a.toString, (a + 1).toChar))) take (amount % 27) toSet
  }

  val repo = mock[SesameRepository]
  val blacklistService = new BlacklistService(repo)
  val timetableService = new TimetableService(blacklistService)
  val scheduleService = new ScheduleService(timetableService)

  val ft = DateTimeFormat.forPattern("HH:mm:ss")
  val fd = DateTimeFormat.forPattern("dd/MM/yyyy")

  def gen(specs: Vector[(Timetable, Set[Group], AssignmentPlan)]): Vector[((Gen[ScheduleG, Conflict, Int], Int), Evaluation)] = {
    def tryGen(t: Timetable, g: Set[Group], ap: AssignmentPlan, comp: Vector[ScheduleG]): ((Gen[ScheduleG, Conflict, Int], Int), Evaluation) = {
      val result = scheduleService.generate(t, g, ap, comp)
      val eval = scheduleService.evaluate(result._1.elem, ap.numberOfEntries, comp)

      /*if (eval.conflicts.isEmpty)
        (result, eval)
      else {
        tryGen(t, g, ap, comp)
      }*/

      (result, eval)
    }

    specs.foldLeft((Vector.empty[ScheduleG], Vector.empty[((Gen[ScheduleG, Conflict, Int], Int), Evaluation)])) {
      case ((comp, _), (t, g, ap)) =>
        val result = tryGen(t, g, ap, comp)

        (comp ++ Vector(result._1._1.elem), Vector((result._1, result._2)))
    }._2
  }

  when(repo.get[Blacklist](anyObject(), anyObject())).thenReturn(Success(Set.empty[Blacklist]))

  "A ScheduleService" should {

    import TimetableDateEntry._

    "populate initial schedules any times" in {
      val entries = (0 until 6).map(n => TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, Weekday.toDay(n).index, LocalTime.now, LocalTime.now)).toSet
      val timetable = Timetable(Labwork.randomUUID, entries, LocalDate.now, Blacklist.empty, Timetable.randomUUID)
      val planEntries = (0 until 5).map(n => AssignmentEntry(n, Set.empty)).toSet
      val plan = AssignmentPlan(planEntries.size, planEntries)
      val groups = alph(8).map(a => Group(a, UUID.randomUUID(), Set.empty))

      val times = 100
      val result = scheduleService.populate(times, timetable, plan, groups)
      //println(result.head.entries.toVector.sortBy(toLocalDateTime).map(_.group.label).take(groups.size))
      //println(result.last.entries.toVector.sortBy(toLocalDateTime).map(_.group.label).take(groups.size))

      result.nonEmpty shouldBe true
      result.size shouldBe times

      result.foldLeft((true, result.head)) {
        case ((b, p), n) =>
          val prev = p.entries.toVector.sortBy(toLocalDateTime).map(_.group.label).take(groups.size)
          val next = n.entries.toVector.sortBy(toLocalDateTime).map(_.group.label).take(groups.size)
          (b && prev == next, n)
      }._1 shouldBe false

      result.foreach(f => f.entries.size shouldBe plan.numberOfEntries * groups.size)
      groups.forall { group =>
        result.forall(_.entries.count(_.group.id == group.id) == plan.numberOfEntries)
      } shouldBe true

      val g = result.map(_.entries.toVector.sortBy(toLocalDateTime).map(_.group.label).grouped(groups.size).toVector)
      val gv = groups.toVector.map(_.id)
      g.forall(vec => vec.foldLeft((true, vec.head)) {
        case ((b, rep), l) => (b && l == rep, rep)
      }._1) shouldBe true
      result.forall(_.entries.toVector.sortBy(toLocalDateTime).map(_.group.id).grouped(groups.size).toVector.forall(v1 => v1.forall(gv.contains) && v1.size == gv.size)) shouldBe true
    }

    "mutate given schedule by swapping two randomly chosen groups" in {
      import scala.util.Random._

      val plan = AssignmentPlan(8, Set.empty)
      val groups = alph(8).map(Group(_, UUID.randomUUID(), Set.empty))
      val entries = (0 until plan.numberOfEntries * groups.size).grouped(groups.size).flatMap(_.zip(groups)).map {
        case (n, group) =>
        val date = LocalDate.now.plusWeeks(n)
        val start = LocalTime.now.withHourOfDay(nextInt(19))
        val end = start.plusHours(nextInt(3))

        ScheduleEntryG(start, end, date, Room.randomUUID, Employee.randomUUID, group, ScheduleEntry.randomUUID)
      }.toSet
      val schedule = ScheduleG(UUID.randomUUID(), entries, UUID.randomUUID())

      val result = (0 until 100).map(_ => scheduleService.mutate(schedule)).toVector

      /*schedule.entries.toVector.sortBy(toLocalDateTime).map(_.group.label).grouped(groups.size).foreach(println)
      println("=========================")
      result.foreach(_.entries.toVector.sortBy(toLocalDateTime).map(_.group.label).grouped(groups.size).toVector.foreach(println))*/

      result.foreach(_.entries.toVector.sortBy(toLocalDateTime) should not be schedule.entries.toVector.sortBy(toLocalDateTime))
      val g = result.map(_.entries.toVector.sortBy(toLocalDateTime).map(_.group.label).grouped(groups.size).toVector)
      val gv = groups.toVector.map(_.id)
      g.forall(vec => vec.foldLeft((true, vec.head)) {
        case ((b, rep), l) => (b && l == rep, rep)
      }._1) shouldBe true
      result.forall(_.entries.toVector.sortBy(toLocalDateTime).map(_.group.id).grouped(groups.size).toVector.forall(v1 => v1.forall(gv.contains) && v1.size == gv.size)) shouldBe true
    }

    "mutate given schedule destructively by rearrange contents of groups for one conflict" in {
      import scala.util.Random._

      val sb = Group.randomUUID
      val nb = Group.randomUUID
      val ok1 = Group.randomUUID
      val ok2 = Group.randomUUID
      val groups = Set(
        Group("A", Labwork.randomUUID, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), sb),
        Group("B", Labwork.randomUUID, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), ok1),
        Group("C", Labwork.randomUUID, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), ok2),
        Group("D", Labwork.randomUUID, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), nb)
      )
      val plan = AssignmentPlan(8, Set.empty)
      val entries = (0 until plan.numberOfEntries * groups.size).grouped(groups.size).flatMap(_.zip(groups)).map {
        case (n, group) =>
          val date = LocalDate.now.plusWeeks(n)
          val start = LocalTime.now.withHourOfDay(nextInt(19))
          val end = start.plusHours(nextInt(3))

          ScheduleEntryG(start, end, date, Room.randomUUID, Employee.randomUUID, group, ScheduleEntry.randomUUID)
      }.toSet
      val schedule = ScheduleG(UUID.randomUUID(), entries, UUID.randomUUID())

      val ce = schedule.entries.find(_.group.id == sb).get
      val g = groups.find(_.id == sb).get
      val m = g.members.toVector
      val s = Vector(m(1), m(3), m(5))

      val result = scheduleService.mutateDestructive(schedule, Vector(Conflict(ce, s, g)))

      result should not be schedule
      result.entries.find(_.group.id == sb).foreach(a => s.foreach(s => a.group.members.find(_ == s) shouldBe None))
      result.entries.find(_.group.id == nb).foreach(a => s.foreach(s => a.group.members.find(_ == s) shouldBe Some(s)))
      result.entries.find(_.group.id == nb).foreach(_.group.members should not be groups.find(_.id == nb).get.members)
      result.entries.find(_.group.id == sb).foreach(_.group.members should not be groups.find(_.id == sb).get.members)
      result.entries.find(_.group.id == ok1).foreach(_.group.members shouldBe groups.find(_.id == ok1).get.members)
      result.entries.find(_.group.id == ok2).foreach(_.group.members shouldBe groups.find(_.id == ok2).get.members)

      result.entries.count(_.group.id == sb) shouldBe plan.numberOfEntries
      result.entries.count(_.group.id == nb) shouldBe plan.numberOfEntries
      result.entries.count(_.group.id == ok1) shouldBe plan.numberOfEntries
      result.entries.count(_.group.id == ok2) shouldBe plan.numberOfEntries

      result.entries.toVector.flatMap(_.group.members).diff(schedule.entries.toVector.flatMap(_.group.members)) shouldBe empty
      result.entries.toVector.flatMap(_.group.members).forall(a => schedule.entries.toVector.flatMap(_.group.members).count(_ == a) == plan.numberOfEntries) shouldBe true

      val gg = result.entries.toVector.sortBy(toLocalDateTime).map(_.group.label).grouped(groups.size).toVector
      val gv = groups.toVector.map(_.id)
      gg.foldLeft((true, gg.head)) {
        case ((b, rep), l) => (b && l == rep, rep)
      }._1 shouldBe true
      result.entries.toVector.sortBy(toLocalDateTime).map(_.group.id).grouped(groups.size).toVector.forall(v1 => v1.forall(gv.contains) && v1.size == gv.size) shouldBe true
    }

    "mutate given schedule destructively by rearrange contents of groups for multiple conflicts" in {
      val sb1 = Group.randomUUID
      val nb1 = Group.randomUUID
      val sb2 = Group.randomUUID
      val ok = Group.randomUUID
      val groups = Set(
        Group("A", Labwork.randomUUID, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), sb1),
        Group("B", Labwork.randomUUID, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), sb2),
        Group("C", Labwork.randomUUID, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), ok),
        Group("D", Labwork.randomUUID, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), nb1)
      )
      val plan = AssignmentPlan(8, Set.empty)
      val entries = (0 until plan.numberOfEntries * groups.size).grouped(groups.size).flatMap(_.zip(groups)).map {
        case (n, group) =>
          val date = LocalDate.now.plusWeeks(n)
          val start = LocalTime.now.withHourOfDay(nextInt(19))
          val end = start.plusHours(nextInt(3))

          ScheduleEntryG(start, end, date, Room.randomUUID, Employee.randomUUID, group, ScheduleEntry.randomUUID)
      }.toSet
      val schedule = ScheduleG(UUID.randomUUID(), entries, UUID.randomUUID())

      val ce1 = schedule.
        entries.find(_.group.id == sb1).get
      val g1 = groups.find(_.id == sb1).get
      val m1 = g1.members.toVector
      val s1 = Vector(m1(1), m1(3), m1(5))

      val ce2 = schedule.entries.find(_.group.id == sb2).get
      val g2 = groups.find(_.id == sb2).get
      val m2 = g2.members.toVector
      val s2 = Vector(m2(2), m2(4))

      val result = scheduleService.mutateDestructive(schedule, Vector(
        Conflict(ce1, s1, g1),
        Conflict(ce2, s2, g2)
      ))

      result should not be schedule
      result.entries.find(_.group.id == sb1).foreach(a => s1.foreach(s => a.group.members.find(_ == s) shouldBe None))
      result.entries.find(_.group.id == nb1).foreach(a => s1.foreach(s => a.group.members.find(_ == s) shouldBe Some(s)))
      result.entries.find(_.group.id == sb2).foreach(a => s2.foreach(s => a.group.members.find(_ == s) shouldBe None))

      result.entries.find(_.group.id == nb1).foreach(_.group.members should not be groups.find(_.id == nb1).get.members)
      result.entries.find(_.group.id == sb1).foreach(_.group.members should not be groups.find(_.id == sb1).get.members)
      result.entries.find(_.group.id == sb2).foreach(_.group.members should not be groups.find(_.id == sb2).get.members)
      result.entries.find(_.group.id == ok).foreach(_.group.members shouldBe groups.find(_.id == ok).get.members)

      result.entries.count(_.group.id == sb1) shouldBe plan.numberOfEntries
      result.entries.count(_.group.id == nb1) shouldBe plan.numberOfEntries
      result.entries.count(_.group.id == sb2) shouldBe plan.numberOfEntries
      result.entries.count(_.group.id == ok) shouldBe plan.numberOfEntries

      result.entries.toVector.flatMap(_.group.members).diff(schedule.entries.toVector.flatMap(_.group.members)) shouldBe empty
      result.entries.toVector.flatMap(_.group.members).forall(a => schedule.entries.toVector.flatMap(_.group.members).count(_ == a) == plan.numberOfEntries) shouldBe true

      val gg = result.entries.toVector.sortBy(toLocalDateTime).map(_.group.label).grouped(groups.size).toVector
      val gv = groups.toVector.map(_.id)
      gg.foldLeft((true, gg.head)) {
        case ((b, rep), l) => (b && l == rep, rep)
      }._1 shouldBe true
      result.entries.toVector.sortBy(toLocalDateTime).map(_.group.id).grouped(groups.size).toVector.forall(v1 => v1.forall(gv.contains) && v1.size == gv.size) shouldBe true
    }

    "successfully cross two schedules" in {
      import scala.util.Random._

      val plan = AssignmentPlan(8, Set.empty)
      val groups = alph(8).map(Group(_, UUID.randomUUID(), Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID)))
      val g1 = shuffle(groups.toVector)
      val g2 = shuffle(groups.toVector)

      val entries = {
        (0 until plan.numberOfEntries * groups.size).grouped(groups.size).flatMap(_.zip(g1).zip(g2).map {
          case (n, group) =>
            val date = LocalDate.now.plusWeeks(n._1)
            val start = LocalTime.now.withHourOfDay(nextInt(19))
            val end = start.plusHours(nextInt(3))

            (ScheduleEntryG(start, end, date, Room.randomUUID, Employee.randomUUID, n._2, ScheduleEntry.randomUUID), ScheduleEntryG(start, end, date, Room.randomUUID, Employee.randomUUID, group, ScheduleEntry.randomUUID))
        }).toSet
      }.unzip

      val left = ScheduleG(UUID.randomUUID(), entries._1, UUID.randomUUID())
      val right = ScheduleG(UUID.randomUUID(), entries._2, UUID.randomUUID())
      val cLeft = Conflict(left.entries.head, left.entries.head.group.members.toVector.take(3), left.entries.head.group)
      val cRight = Conflict(right.entries.head, right.entries.head.group.members.toVector.take(3), right.entries.head.group)

      val result = scheduleService.crossover((left, List(cLeft)), (right, List(cRight)))

      left should not be result._1
      right should not be result._2

      result._1.entries.find(_.group == cLeft.group).forall(a => left.entries.find(_.group == a.group).contains(a)) shouldBe false
      result._2.entries.find(_.group == cRight.group).forall(a => right.entries.find(_.group == a.group).contains(a)) shouldBe false

      result._1.entries.zip(left.entries).count(e => e._1 == e._2) < left.entries.size shouldBe true
      result._2.entries.zip(right.entries).count(e => e._1 == e._2) < right.entries.size shouldBe true

      val leftCount = result._1.entries.count(c => left.entries.contains(c))
      leftCount != left.entries.size && leftCount < left.entries.size && leftCount > 0 && leftCount > left.entries.size / 2 shouldBe true

      val rightCount = result._2.entries.count(c => right.entries.contains(c))
      rightCount != right.entries.size && rightCount < right.entries.size && rightCount > 0 && rightCount > right.entries.size / 2 shouldBe true

      Vector(result._1, result._2).foreach { s =>
        val gg = s.entries.toVector.sortBy(toLocalDateTime).map(_.group.label).grouped(groups.size).toVector
        val gv = groups.toVector.map(_.id)
        gg.foldLeft((true, gg.head)) {
          case ((b, rep), l) => (b && l == rep, rep)
        }._1 shouldBe true
        s.entries.toVector.sortBy(toLocalDateTime).map(_.group.id).grouped(groups.size).toVector.forall(v1 => v1.forall(gv.contains) && v1.size == gv.size) shouldBe true
      }
    }

    "evaluate an given schedule when there are no other schedules" in {
      val planEntries = (0 until 5).map(n => AssignmentEntry(n, Set.empty[EntryType])).toSet
      val plan = AssignmentPlan(planEntries.size, planEntries)
      val labwork = Labwork("label", "description", Semester.randomUUID, Course.randomUUID, Degree.randomUUID, plan)
      val entries = (0 until 6).map(n => TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, Weekday.toDay(n).index, LocalTime.now, LocalTime.now)).toSet
      val timetable = Timetable(labwork.id, entries, LocalDate.now, Blacklist.empty, Timetable.randomUUID)

      val groups = Set(
        Group("A", labwork.id, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), Group.randomUUID),
        Group("B", labwork.id, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), Group.randomUUID),
        Group("C", labwork.id, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), Group.randomUUID)
      )
      val existing = Vector.empty[ScheduleG]

      val schedule = scheduleService.populate(1, timetable, plan, groups).head
      val result = scheduleService.evaluate(schedule, plan.numberOfEntries, existing)

      result.conflicts shouldBe empty
      result.value shouldBe 0
    }

    "evaluate an given schedule when there are some other schedules" in {
      val planEntries = (0 until 8).map(n => AssignmentEntry(n, Set.empty[EntryType])).toSet
      val plan = AssignmentPlan(planEntries.size, planEntries)
      val mi = Degree("mi", Degree.randomUUID)
      val ap1 = Course("ap1", "c1", Employee.randomUUID, 1, Course.randomUUID)
      val ma1 = Course("ma1", "c2", Employee.randomUUID, 1, Course.randomUUID)
      val semester1 = Semester("semester1", "start1", "end1", "exam1", Blacklist.empty, Semester.randomUUID)
      val ap1Prak = Labwork("ap1Prak", "desc1", semester1.id, ap1.id, mi.id, plan)
      val ma1Prak = Labwork("ma1Prak", "desc2", semester1.id, ma1.id, mi.id, plan)

      val ap1Entries = Set(
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("27/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("09:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("27/10/2015")).index, ft.parseLocalTime("09:00:00"), ft.parseLocalTime("10:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("27/10/2015")).index, ft.parseLocalTime("10:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("14:00:00"), ft.parseLocalTime("15:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("14:00:00"), ft.parseLocalTime("15:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("15:00:00"), ft.parseLocalTime("16:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("15:00:00"), ft.parseLocalTime("16:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("16:00:00"), ft.parseLocalTime("17:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("16:00:00"), ft.parseLocalTime("17:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("09:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("09:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("09:00:00"), ft.parseLocalTime("10:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("09:00:00"), ft.parseLocalTime("10:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("10:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("10:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("12:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("12:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("12:00:00"), ft.parseLocalTime("13:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("12:00:00"), ft.parseLocalTime("13:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("13:00:00"), ft.parseLocalTime("14:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("13:00:00"), ft.parseLocalTime("14:00:00"))
      )
      val ma1Entries = Set(
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("26/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("27/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("14:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("14:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("14:00:00"), ft.parseLocalTime("17:00:00"))
      )

      import scala.util.Random._
      val students = (0 until 300).map(_ => Student.randomUUID).toVector
      val ap1G = shuffle(students).take(250).grouped(10).map(s => Group("", ap1Prak.id, s.toSet, Group.randomUUID)).toSet
      val ma1G = shuffle(students).take(240).grouped(20).map(s => Group("", ma1Prak.id, s.toSet, Group.randomUUID)).toSet

      val ap1T = Timetable(ap1Prak.id, ap1Entries, fd.parseLocalDate("27/10/2015"), Blacklist.empty, Timetable.randomUUID)
      val ma1T = Timetable(ma1Prak.id, ma1Entries, fd.parseLocalDate("26/10/2015"), Blacklist.empty, Timetable.randomUUID)

      val ap1Schedule = scheduleService.populate(1, ap1T, plan, ap1G)
      val ma1Schedule = scheduleService.populate(1, ma1T, plan, ma1G).head

      val result = scheduleService.evaluate(ma1Schedule, plan.numberOfEntries, ap1Schedule)
      println(s"conflicts ${result.conflicts.size}")
      println(s"guys ${result.conflicts.map(_.member)}")
      println(s"date ${result.conflicts.map(e => e.entry.date.toLocalDateTime(e.entry.start))}")
      println(s"value ${result.value}")

      result.conflicts should not be empty
      result.value should be < 1000
      result.conflicts.size should be <= result.value
      result.conflicts.forall(c => ma1G.contains(c.group) && c.member.forall(u => ma1G.exists(_.members.contains(u))) && ma1Schedule.entries.contains(c.entry)) shouldBe true
      result.conflicts.forall(c => ap1G.contains(c.group) && c.member.forall(u => ap1G.exists(_.members.contains(u))) && ap1Schedule.head.entries.contains(c.entry)) shouldBe false
    }
  }

  "A ScheduleGenesisService" should {

    "generate an collision free schedule instantly" in {
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
      val mi = Degree("mi", Degree.randomUUID)
      val ap1 = Course("ap1", "c1", Employee.randomUUID, 1, Course.randomUUID)
      val semester1 = Semester("semester1", "start1", "end1", "exam1", Blacklist.empty, Semester.randomUUID)
      val ap1Prak = Labwork("ap1Prak", "desc1", semester1.id, ap1.id, mi.id, ap1Plan)

      val ap1Entries = Set(
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("27/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("09:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("27/10/2015")).index, ft.parseLocalTime("09:00:00"), ft.parseLocalTime("10:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("27/10/2015")).index, ft.parseLocalTime("10:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("14:00:00"), ft.parseLocalTime("15:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("14:00:00"), ft.parseLocalTime("15:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("15:00:00"), ft.parseLocalTime("16:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("15:00:00"), ft.parseLocalTime("16:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("16:00:00"), ft.parseLocalTime("17:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("16:00:00"), ft.parseLocalTime("17:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("09:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("09:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("09:00:00"), ft.parseLocalTime("10:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("09:00:00"), ft.parseLocalTime("10:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("10:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("10:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("12:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("12:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("12:00:00"), ft.parseLocalTime("13:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("12:00:00"), ft.parseLocalTime("13:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("13:00:00"), ft.parseLocalTime("14:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("13:00:00"), ft.parseLocalTime("14:00:00"))
      )

      import scala.util.Random._
      val students = (0 until 200).map(_ => Student.randomUUID).toVector
      val ap1G = shuffle(students).take(200).grouped(10).map(s => Group("", ap1Prak.id, s.toSet, Group.randomUUID)).toSet

      val ap1T = Timetable(ap1Prak.id, ap1Entries, fd.parseLocalDate("27/10/2015"), Blacklist.empty, Timetable.randomUUID)

      val result = gen(Vector(
        (ap1T, ap1G, ap1Plan)
      )).head

      println(s"gen ${result._1._2}")
      println(s"conflict size ${result._2.conflicts.size}")
      println(s"conflict value ${result._2.value}")
      result._2.conflicts shouldBe empty
      result._2.value shouldBe 0

      result._1._2 shouldBe 0
      result._1._1.evaluate.err shouldBe empty
      result._1._1.evaluate.value shouldBe 0
      result._1._1.elem.labwork shouldEqual ap1Prak.id
      result._1._1.elem.entries.groupBy(_.group) forall {
        case (_, ss) => ss.size == ap1Plan.numberOfEntries
      } shouldBe true
    }

    "generate an collision free schedule in few generations even with one existing competitive schedule and more density" in {
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
      val mi = Degree("mi", Degree.randomUUID)
      val ap1 = Course("ap1", "c1", Employee.randomUUID, 1, Course.randomUUID)
      val ma1 = Course("ma1", "c2", Employee.randomUUID, 1, Course.randomUUID)
      val semester1 = Semester("semester1", "start1", "end1", "exam1", Blacklist.empty, Semester.randomUUID)
      val ap1Prak = Labwork("ap1Prak", "desc1", semester1.id, ap1.id, mi.id, ap1Plan)
      val ma1Prak = Labwork("ma1Prak", "desc2", semester1.id, ma1.id, mi.id, ma1Plan)

      val ap1Entries = Set(
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("27/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("09:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("27/10/2015")).index, ft.parseLocalTime("09:00:00"), ft.parseLocalTime("10:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("27/10/2015")).index, ft.parseLocalTime("10:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("14:00:00"), ft.parseLocalTime("15:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("14:00:00"), ft.parseLocalTime("15:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("15:00:00"), ft.parseLocalTime("16:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("15:00:00"), ft.parseLocalTime("16:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("16:00:00"), ft.parseLocalTime("17:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("16:00:00"), ft.parseLocalTime("17:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("09:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("09:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("09:00:00"), ft.parseLocalTime("10:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("09:00:00"), ft.parseLocalTime("10:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("10:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("10:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("12:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("12:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("12:00:00"), ft.parseLocalTime("13:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("12:00:00"), ft.parseLocalTime("13:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("13:00:00"), ft.parseLocalTime("14:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("13:00:00"), ft.parseLocalTime("14:00:00"))
      )
      val ma1Entries = Set(
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("26/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("27/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("14:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("14:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("14:00:00"), ft.parseLocalTime("17:00:00"))
      )

      import scala.util.Random._
      val students = (0 until 200).map(_ => Student.randomUUID).toVector
      val ap1G = shuffle(students).take(180).grouped(10).map(s => Group("", ap1Prak.id, s.toSet, Group.randomUUID)).toSet
      val ma1G = shuffle(students).take(180).grouped(20).map(s => Group("", ma1Prak.id, s.toSet, Group.randomUUID)).toSet

      val ap1T = Timetable(ap1Prak.id, ap1Entries, fd.parseLocalDate("27/10/2015"), Blacklist.empty, Timetable.randomUUID)
      val ma1T = Timetable(ma1Prak.id, ma1Entries, fd.parseLocalDate("26/10/2015"), Blacklist.empty, Timetable.randomUUID)

      val result = gen(Vector(
        (ap1T, ap1G, ap1Plan),
        (ma1T, ma1G, ma1Plan)
      )).head

      println(s"gen ${result._1._2}")
      println(s"conflict size ${result._2.conflicts.size}")
      println(s"conflict value ${result._2.value}")
      result._2.conflicts shouldBe empty
      result._2.value shouldBe 0

      result._1._2 should be >= 0
      result._1._1.evaluate.err shouldBe empty
      result._1._1.evaluate.value shouldBe 0
      result._1._1.elem.labwork shouldEqual ma1Prak.id
      result._1._1.elem.entries.groupBy(_.group) forall {
        case (_, ss) => ss.size == ma1Plan.numberOfEntries
      } shouldBe true
    }

    /*"generate yet another collision free schedule in few generations even with two existing competitive schedule and more density" in {
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
      val mi = Degree("mi", Degree.randomUUID)
      val ap1 = Course("ap1", "c1", Employee.randomUUID, 1, Course.randomUUID)
      val ma1 = Course("ma1", "c2", Employee.randomUUID, 1, Course.randomUUID)
      val gdvk = Course("gdvk", "c3", Employee.randomUUID, 1, Course.randomUUID)
      val semester1 = Semester("semester1", "start1", "end1", "exam1", Blacklist.empty, Semester.randomUUID)
      val ap1Prak = Labwork("ap1Prak", "desc1", semester1.id, ap1.id, mi.id, ap1Plan)
      val ma1Prak = Labwork("ma1Prak", "desc2", semester1.id, ma1.id, mi.id, ma1Plan)
      val gdvkPrak = Labwork("gdvkPrak", "desc3", semester1.id, gdvk.id, mi.id, gdvkPlan)

      val ap1Entries = Set(
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("27/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("09:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("27/10/2015")).index, ft.parseLocalTime("09:00:00"), ft.parseLocalTime("10:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("27/10/2015")).index, ft.parseLocalTime("10:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("14:00:00"), ft.parseLocalTime("15:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("14:00:00"), ft.parseLocalTime("15:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("15:00:00"), ft.parseLocalTime("16:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("15:00:00"), ft.parseLocalTime("16:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("16:00:00"), ft.parseLocalTime("17:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("16:00:00"), ft.parseLocalTime("17:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("09:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("09:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("09:00:00"), ft.parseLocalTime("10:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("09:00:00"), ft.parseLocalTime("10:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("10:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("10:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("12:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("12:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("12:00:00"), ft.parseLocalTime("13:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("12:00:00"), ft.parseLocalTime("13:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("13:00:00"), ft.parseLocalTime("14:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("13:00:00"), ft.parseLocalTime("14:00:00"))
      )
      val ma1Entries = Set(
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("26/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("27/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("14:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("14:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("14:00:00"), ft.parseLocalTime("17:00:00"))
      )
      val gdvkEntries = Set(
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("13:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("14:00:00"), ft.parseLocalTime("17:00:00"))
      )

      import scala.util.Random._
      val students = (0 until 200).map(_ => Student.randomUUID).toVector
      val ap1G = shuffle(students).take(180).grouped(10).map(s => Group("", ap1Prak.id, s.toSet, Group.randomUUID)).toSet
      val ma1G = shuffle(students).take(180).grouped(20).map(s => Group("", ma1Prak.id, s.toSet, Group.randomUUID)).toSet
      val gdvkG = shuffle(students).take(150).grouped(30).map(s => Group("", gdvkPrak.id, s.toSet, Group.randomUUID)).toSet

      val ap1T = Timetable(ap1Prak.id, ap1Entries, fd.parseLocalDate("27/10/2015"), Blacklist.empty, Timetable.randomUUID)
      val ma1T = Timetable(ma1Prak.id, ma1Entries, fd.parseLocalDate("26/10/2015"), Blacklist.empty, Timetable.randomUUID)
      val gdvkT = Timetable(gdvkPrak.id, gdvkEntries, fd.parseLocalDate("30/10/2015"), Blacklist.empty, Timetable.randomUUID)

      val result = gen(Vector(
        (ap1T, ap1G, ap1Plan),
        (ma1T, ma1G, ma1Plan),
        (gdvkT, gdvkG, gdvkPlan)
      )).head

      println(s"gen ${result._1._2}")
      println(s"conflict size ${result._2.conflicts.size}")
      println(s"conflict value ${result._2.value}")
      result._2.conflicts shouldBe empty
      result._2.value shouldBe 0

      result._1._2 should be > 0
      result._1._1.evaluate.err shouldBe empty
      result._1._1.evaluate.value shouldBe 0
      result._1._1.elem.labwork shouldEqual gdvkT.id
      result._1._1.elem.entries.groupBy(_.group) forall {
        case (_, ss) => ss.size == gdvkPlan.numberOfEntries
      } shouldBe true
    }*/

    /*"generate n schedules with competitive ones and more density" in {
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
      val mi = Degree("mi", Degree.randomUUID)
      val ap1 = Course("ap1", "c1", Employee.randomUUID, 1, Course.randomUUID)
      val ma1 = Course("ma1", "c2", Employee.randomUUID, 1, Course.randomUUID)
      val gdvk = Course("gdvk", "c3", Employee.randomUUID, 1, Course.randomUUID)
      val another = Course("another", "c4", Employee.randomUUID, 1, Course.randomUUID)
      val semester1 = Semester("semester1", "start1", "end1", "exam1", Blacklist.empty, Semester.randomUUID)
      val ap1Prak = Labwork("ap1Prak", "desc1", semester1.id, ap1.id, mi.id, ap1Plan)
      val ma1Prak = Labwork("ma1Prak", "desc2", semester1.id, ma1.id, mi.id, ma1Plan)
      val gdvkPrak = Labwork("gdvkPrak", "desc3", semester1.id, gdvk.id, mi.id, gdvkPlan)
      val anotherPrak = Labwork("anotherPrak", "desc4", semester1.id, another.id, mi.id, anotherPlan)

      val ap1Entries = Set(
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("27/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("09:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("27/10/2015")).index, ft.parseLocalTime("09:00:00"), ft.parseLocalTime("10:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("27/10/2015")).index, ft.parseLocalTime("10:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("14:00:00"), ft.parseLocalTime("15:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("14:00:00"), ft.parseLocalTime("15:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("15:00:00"), ft.parseLocalTime("16:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("15:00:00"), ft.parseLocalTime("16:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("16:00:00"), ft.parseLocalTime("17:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("16:00:00"), ft.parseLocalTime("17:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("09:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("09:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("09:00:00"), ft.parseLocalTime("10:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("09:00:00"), ft.parseLocalTime("10:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("10:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("10:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("12:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("12:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("12:00:00"), ft.parseLocalTime("13:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("12:00:00"), ft.parseLocalTime("13:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("13:00:00"), ft.parseLocalTime("14:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("13:00:00"), ft.parseLocalTime("14:00:00"))
      )
      val ma1Entries = Set(
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("26/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("27/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("14:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("14:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("14:00:00"), ft.parseLocalTime("17:00:00"))
      )
      val gdvkEntries = Set(
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("13:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("14:00:00"), ft.parseLocalTime("17:00:00"))
      )
      val anotherEntries = Set(
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("26/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("13:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("28/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("13:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("28/10/2015")).index, ft.parseLocalTime("13:00:00"), ft.parseLocalTime("15:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("13:00:00")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("13:00:00"), ft.parseLocalTime("15:00:00"))
      )

      import scala.util.Random._
      val students = (0 until 300).map(_ => Student.randomUUID).toVector
      val ap1G = shuffle(students).take(280).grouped(10).map(s => Group("", ap1Prak.id, s.toSet, Group.randomUUID)).toSet
      val ma1G = shuffle(students).take(280).grouped(20).map(s => Group("", ma1Prak.id, s.toSet, Group.randomUUID)).toSet
      val gdvkG = shuffle(students).take(270).grouped(30).map(s => Group("", gdvkPrak.id, s.toSet, Group.randomUUID)).toSet
      val anotherG = shuffle(students).take(200).grouped(20).map(s => Group("", anotherPrak.id, s.toSet, Group.randomUUID)).toSet


      val ap1T = Timetable(ap1Prak.id, ap1Entries, fd.parseLocalDate("27/10/2015"), Blacklist.empty, Timetable.randomUUID)
      val ma1T = Timetable(ma1Prak.id, ma1Entries, fd.parseLocalDate("26/10/2015"), Blacklist.empty, Timetable.randomUUID)
      val gdvkT = Timetable(gdvkPrak.id, gdvkEntries, fd.parseLocalDate("30/10/2015"), Blacklist.empty, Timetable.randomUUID)
      val anotherT = Timetable(anotherPrak.id, anotherEntries, fd.parseLocalDate("26/10/2015"), Blacklist.empty, Timetable.randomUUID)

      val result = gen(Vector(
        (ap1T, ap1G, ap1Plan),
        (ma1T, ma1G, ma1Plan),
        (gdvkT, gdvkG, gdvkPlan),
        (anotherT, anotherG, anotherPlan)
      )).head

      println(s"gen ${result._1._2}")
      println(s"conflict size ${result._2.conflicts.size}")
      println(s"conflict value ${result._2.value}")
      result._2.conflicts shouldBe empty
      result._2.value shouldBe 0

      result._1._2 should be > 0
      result._1._1.evaluate.err shouldBe empty
      result._1._1.evaluate.value shouldBe 0
      result._1._1.elem.labwork shouldEqual anotherPrak.id
      result._1._1.elem.entries.groupBy(_.group) forall {
        case (_, ss) => ss.size == anotherPlan.numberOfEntries
      } shouldBe true
    }*/
  }
}
