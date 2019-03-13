package services

/*
import java.util.UUID

import base.TestBaseDefinition
import models.{genesis, _}
import models.genesis.{Conflict, ScheduleEntryGen, ScheduleGen}
import org.joda.time.{LocalDate, LocalTime, Weeks}
import org.joda.time.format.DateTimeFormat
import org.scalatest.WordSpec
import utils.Evaluation

import scala.language.postfixOps
import scala.util.Random._
import utils.Ops.MonoidInstances._

final class ScheduleServiceSpec extends WordSpec with TestBaseDefinition {
  import dao.AbstractDaoSpec._

  "A ScheduleService" should {

    "return empty list of scheduleG's when there are no competitive schedules" in {} // TODO
    "return scheduleG's when there are competitive schedules" in {} // TODO

    "populate initial schedules any times" in {
      val times = 100
      val extrapolated = TimetableService.extrapolateTimetableByWeeks(timetable, weeks, Vector.empty, plan, groups.size)
      val result = scheduleService.population(times, labId, extrapolated, groups)
      //println(result.head.entries.toVector.sortBy(toLocalDateTime).map(_.group.label).take(groups.size))
      //println(result.last.entries.toVector.sortBy(toLocalDateTime).map(_.group.label).take(groups.size))

      result.nonEmpty shouldBe true
      result.size shouldBe times

      result.foldLeft((true, result.head)) {
        case ((b, p), n) =>
          val prev = p.sorted.map(_.group.label).take(groups.size)
          val next = n.sorted.map(_.group.label).take(groups.size)
          (b && prev == next, n)
      }._1 shouldBe false

      result.foreach(f => f.entries.size shouldBe plan.entries.size * groups.size)
      groups.forall { group =>
        result.forall(_.entries.count(_.group.id == group.id) == plan.entries.size)
      } shouldBe true

      val g = result.map(_.sorted.map(_.group.label).grouped(groups.size).toVector)
      val gv = groups.map(_.id)
      g.forall(vec => vec.foldLeft((true, vec.head)) {
        case ((b, rep), l) => (b && l == rep, rep)
      }._1) shouldBe true
      result.forall(_.sorted.map(_.group.id).grouped(groups.size).toVector.forall(v1 => v1.forall(gv.contains) && v1.size == gv.size)) shouldBe true
    }

    "mutate given schedule by swapping two randomly chosen groups" in {
      import scala.util.Random._
      val schedule = {
        val entries = (0 until plan.entries.size * groups.size).grouped(groups.size).flatMap(_.zip(groups)).map {
          case (n, group) =>
            val date = LocalDate.now.plusWeeks(n)
            val start = LocalTime.now.withHourOfDay(nextInt(19))
            val end = start.plusHours(nextInt(3))

            ScheduleEntryGen(start, end, date, UUID.randomUUID, Set(User.randomUUID), group)
        }.toVector

        ScheduleGen(labId, entries)
      }

      val result = (0 until 100).map(_ => scheduleService.mutate(schedule, emptyEval)).toVector

      /*schedule.entries.toVector.sortBy(toLocalDateTime).map(_.group.label).grouped(groups.size).foreach(println)
      println("=========================")
      result.foreach(_.entries.toVector.sortBy(toLocalDateTime).map(_.group.label).grouped(groups.size).toVector.foreach(println))*/

      result.foreach(_.sorted should not be schedule.sorted)
      val g = result.map(_.sorted.map(_.group.label).grouped(groups.size).toVector)
      val gv = groups.map(_.id)
      g.forall(vec => vec.foldLeft((true, vec.head)) {
        case ((b, rep), l) => (b && l == rep, rep)
      }._1) shouldBe true
      result.forall(_.sorted.map(_.group.id).grouped(groups.size).toVector.forall(v1 => v1.forall(gv.contains) && v1.size == gv.size)) shouldBe true
    }

    "successfully cross two schedules" in {
      import scala.util.Random._

      val g1 = shuffle(groups)
      val g2 = shuffle(groups)

      val entries = {
        (0 until plan.entries.size * groups.size).grouped(groups.size).flatMap(_.zip(g1).zip(g2).map {
          case (n, group) =>
            val date = LocalDate.now.plusWeeks(n._1)
            val start = LocalTime.now.withHourOfDay(nextInt(19))
            val end = start.plusHours(nextInt(3))

            (ScheduleEntryGen(start, end, date, UUID.randomUUID, Set(User.randomUUID), n._2), ScheduleEntryGen(start, end, date, UUID.randomUUID, Set(User.randomUUID), group))
        }).toVector
      }.unzip

      val left = genesis.ScheduleGen(labId, entries._1)
      val right = genesis.ScheduleGen(labId, entries._2)
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
        val gg = s.sorted.map(_.group.label).grouped(groups.size).toVector
        val gv = groups.map(_.id)
        gg.foldLeft((true, gg.head)) {
          case ((b, rep), l) => (b && l == rep, rep)
        }._1 shouldBe true
        s.sorted.map(_.group.id).grouped(groups.size).toVector.forall(v1 => v1.forall(gv.contains) && v1.size == gv.size) shouldBe true
      }
    }

    "evaluate a given schedule when there are no other schedules" in {
      val existing = Vector.empty[ScheduleGen]

      val extrapolated = TimetableService.extrapolateTimetableByWeeks(timetable, weeks, Vector.empty, plan, groups.size)
      val schedule = scheduleService.population(1, labId, extrapolated, groups).head
      val result = scheduleService.evaluation(existing, plan.entries.size)(schedule)

      result.err shouldBe empty
      result.value shouldBe 0
    }

    "evaluate a given schedule when there are some other schedules" in {
      val ap1Entries = Set(
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("27/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("09:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("27/10/2015")).index, ft.parseLocalTime("09:00:00"), ft.parseLocalTime("10:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("27/10/2015")).index, ft.parseLocalTime("10:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("14:00:00"), ft.parseLocalTime("15:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("14:00:00"), ft.parseLocalTime("15:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("15:00:00"), ft.parseLocalTime("16:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("15:00:00"), ft.parseLocalTime("16:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("16:00:00"), ft.parseLocalTime("17:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("16:00:00"), ft.parseLocalTime("17:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("09:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("09:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("09:00:00"), ft.parseLocalTime("10:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("09:00:00"), ft.parseLocalTime("10:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("10:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("10:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("12:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("12:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("12:00:00"), ft.parseLocalTime("13:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("12:00:00"), ft.parseLocalTime("13:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("13:00:00"), ft.parseLocalTime("14:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("13:00:00"), ft.parseLocalTime("14:00:00"))
      )
      val ma1Entries = Set(
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("26/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("27/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("14:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("14:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("14:00:00"), ft.parseLocalTime("17:00:00"))
      )

      val students = (0 until 300).map(_ => UUID.randomUUID).toVector
      val ap1G = shuffle(students).take(250).grouped(10).map(s => Group("", ap1Prak.id, s.toSet)).toVector
      val ma1G = shuffle(students).take(240).grouped(20).map(s => Group("", ma1Prak.id, s.toSet)).toVector

      val ap1T = Timetable(ap1Prak.id, ap1Entries, fd.parseLocalDate("27/10/2015"), Set.empty)
      val ma1T = Timetable(ma1Prak.id, ma1Entries, fd.parseLocalDate("26/10/2015"), Set.empty)

      val extrapolatedAp1 = TimetableService.extrapolateTimetableByWeeks(ap1T, weeks, Vector.empty, plan, ap1G.size)
      val extrapolatedMa1 = TimetableService.extrapolateTimetableByWeeks(ma1T, weeks, Vector.empty, plan, ma1G.size)
      val ap1Schedule = scheduleService.population(1, ap1Prak.id, extrapolatedAp1, ap1G)
      val ma1Schedule = scheduleService.population(1, ma1Prak.id, extrapolatedMa1, ma1G).head

      val result = scheduleService.evaluation(ap1Schedule, plan.entries.size)(ma1Schedule)
//      println(s"conflicts ${result.err.size}")
//      println(s"guys ${result.err.map(_.members)}")
//      println(s"date ${result.err.map(e => e.entry.date.toLocalDateTime(e.entry.start))}")
//      println(s"value ${result.value}")

      result.err should not be empty
      result.value should be >= 1000
      result.err.size should be <= result.value
      result.err.forall(c => ma1G.contains(c.group) && c.members.forall(u => ma1G.exists(_.members.contains(u))) && ma1Schedule.entries.contains(c.entry)) shouldBe true
      result.err.forall(c => ap1G.contains(c.group) && c.members.forall(u => ap1G.exists(_.members.contains(u))) && ap1Schedule.head.entries.contains(c.entry)) shouldBe false
    }

    def scheduleGen(shuffleGroups: Boolean): ScheduleGen = {
      val groups = alph(10) zip (population(100) grouped 10 toVector) map {
        case (label, group) => Group(label, labId, group toSet)
      }
      val entries = (if (shuffleGroups) shuffle(groups) else groups) map (ScheduleEntryGen(LocalTime.now, LocalTime.now, LocalDate.now, UUID.randomUUID, Set(User.randomUUID), _))
      genesis.ScheduleGen(labId, entries)
    }

    "mutate given schedule destructively by exchanging people between groups" in {
      val schedule = scheduleGen(false)
      val entries = schedule.entries
      val ev = eval(List(Conflict(entries(4), entries(4).group.members take 2 toVector, entries(4).group)))

      val newSchedule = scheduleService.mutateDestructive(schedule, ev)

      val theGroup = entries(4).group
      val theNaughtyOnes = theGroup.members.take(2).toVector

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
      val schedule = scheduleGen(false)
      val entries = schedule.entries
      val ev = eval(List(Conflict(entries(4), entries(4).group.members take 2 toVector, entries(4).group)))

      val newSchedule = scheduleService.mutateDestructive(schedule, ev)

      schedule.entries map (_ group) sortBy (_.label) zip (newSchedule.entries map (_ group) sortBy (_ label)) foreach {
        case ((pgroup, cgroup)) => pgroup.members.size shouldBe cgroup.members.size
      }
    }

    "cross conflicting people destructively with others from different schedules" in {
      val schedule = scheduleGen(true)
      val entries = schedule.entries

      val (schedule1, schedule2) = (schedule, schedule)
      val (e1, e2) = (schedule1.entries(3), schedule2.entries(5))

      val (eval1, eval2) = (Evaluation(List(genesis.Conflict(e1, e1.group.members take 2 toVector, e1.group)), 0),
        Evaluation(List(genesis.Conflict(e2, e2.group.members take 2 toVector, e2.group)), 0))

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
      val schedule = scheduleGen(true)
      val entries = schedule.entries

      val (schedule1, schedule2) = (schedule, schedule)
      val (e1, e2) = (schedule1.entries(3), schedule2.entries(5))

      val (eval1, eval2) = (Evaluation(List(genesis.Conflict(e1, e1.group.members take 2 toVector, e1.group)), 0),
        Evaluation(List(genesis.Conflict(e2, e2.group.members take 2 toVector, e2.group)), 0))

      val (s1, s2) = scheduleService.crossoverDestructive((schedule1, eval1), (schedule2, eval2))

      schedule1.entries map (_ group) sortBy (_ label) zip (s1.entries map (_ group) sortBy (_ label)) foreach {
        case ((pgroup, cgroup)) => pgroup.members.size shouldBe cgroup.members.size
      }

      schedule2.entries map (_ group) sortBy (_ label) zip (s2.entries map (_ group) sortBy (_ label)) foreach {
        case ((pgroup, cgroup)) => pgroup.members.size shouldBe cgroup.members.size
      }
    }
  }

 "A ScheduleGenesisService" should {

    "generate an initial collision free schedule instantly" in {
      val ap1Entries = Set(
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("27/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("09:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("27/10/2015")).index, ft.parseLocalTime("09:00:00"), ft.parseLocalTime("10:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("27/10/2015")).index, ft.parseLocalTime("10:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("14:00:00"), ft.parseLocalTime("15:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("14:00:00"), ft.parseLocalTime("15:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("15:00:00"), ft.parseLocalTime("16:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("15:00:00"), ft.parseLocalTime("16:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("16:00:00"), ft.parseLocalTime("17:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("16:00:00"), ft.parseLocalTime("17:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("09:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("09:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("09:00:00"), ft.parseLocalTime("10:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("09:00:00"), ft.parseLocalTime("10:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("10:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("10:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("12:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("12:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("12:00:00"), ft.parseLocalTime("13:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("12:00:00"), ft.parseLocalTime("13:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("13:00:00"), ft.parseLocalTime("14:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("13:00:00"), ft.parseLocalTime("14:00:00"))
      )

      import scala.util.Random._
      val students = (0 until 200).map(_ => User.randomUUID).toVector
      val ap1G = shuffle(students).take(200).grouped(10).map(s => PostgresGroup("", ap1Prak.id, s.toSet)).toVector

      val ap1T = Timetable(ap1Prak.id, ap1Entries, fd.parseLocalDate("27/10/2015"), Set.empty)

      val result = scheduleService.generate(ap1T, Vector.empty, ap1G, plan, semester, Vector.empty)

      //      println(s"gen ${result._2}")
//      println(s"conflict size ${result._1.evaluate.err.size}")
//      println(s"conflict value ${result._1.evaluate.value}")
      result._1.evaluate.err shouldBe empty
      result._1.evaluate.value shouldBe 0

      result._2 shouldBe 0
      result._1.elem.labwork shouldEqual ap1Prak.id
      result._1.elem.entries.groupBy(_.group) forall {
        case (_, ss) => ss.size == plan.entries.size
      } shouldBe true
    }

    "generate a schedule with minimal or no collisions considering one existing competitive schedule and more density" in {
      val ap1Plan = plan
      val ma1Plan = populateAssignmentPlans(1, 4)(labwork)(_ => 2).head.toUniqueEntity

      val ap1Entries = Set(
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("27/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("09:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("27/10/2015")).index, ft.parseLocalTime("09:00:00"), ft.parseLocalTime("10:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("27/10/2015")).index, ft.parseLocalTime("10:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("14:00:00"), ft.parseLocalTime("15:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("14:00:00"), ft.parseLocalTime("15:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("15:00:00"), ft.parseLocalTime("16:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("15:00:00"), ft.parseLocalTime("16:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("16:00:00"), ft.parseLocalTime("17:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("16:00:00"), ft.parseLocalTime("17:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("09:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("09:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("09:00:00"), ft.parseLocalTime("10:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("09:00:00"), ft.parseLocalTime("10:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("10:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("10:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("12:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("12:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("12:00:00"), ft.parseLocalTime("13:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("12:00:00"), ft.parseLocalTime("13:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("13:00:00"), ft.parseLocalTime("14:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("13:00:00"), ft.parseLocalTime("14:00:00"))
      )
      val ma1Entries = Set(
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("26/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("27/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("11:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("14:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("14:00:00")),
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("14:00:00"), ft.parseLocalTime("17:00:00"))
      )

      val students = (0 until 200).map(_ => User.randomUUID).toVector
      val ap1G = shuffle(students).take(180).grouped(10).map(s => PostgresGroup("", ap1Prak.id, s.toSet)).toVector
      val ma1G = shuffle(students).take(180).grouped(20).map(s => PostgresGroup("", ma1Prak.id, s.toSet)).toVector

      val ap1T = Timetable(ap1Prak.id, ap1Entries, fd.parseLocalDate("27/10/2015"), Set.empty)
      val ma1T = Timetable(ma1Prak.id, ma1Entries, fd.parseLocalDate("26/10/2015"), Set.empty)

      val comp = scheduleService.generate(ap1T, Vector.empty, ap1G, ap1Plan, semester, Vector.empty)._1.elem
      val result = scheduleService.generate(ma1T, Vector.empty, ma1G, ma1Plan, semester, Vector(comp))

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

    "handle deadlocks properly" in {
      val ap1Plan = plan
      val ma1Plan = populateAssignmentPlans(1, 8)(labwork)(_ => 2).head.toUniqueEntity

      val ap1Entries = Set(
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("26/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("09:00:00"))
      )
      val ma1Entries = Set(
        TimetableEntry(Set(User.randomUUID), UUID.randomUUID, Weekday.toDay(fd.parseLocalDate("26/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("11:00:00"))
      )

      val students = (0 until 200).map(_ => User.randomUUID).toVector
      val deadlock = students.take(10).toSet
      val ap1G = Vector(PostgresGroup("", ap1Prak.id, deadlock ++ shuffle(students).take(10).toSet))
      val ma1G = Vector(PostgresGroup("", ma1Prak.id, deadlock ++ shuffle(students).take(10).toSet))

      val ap1T = Timetable(ap1Prak.id, ap1Entries, fd.parseLocalDate("26/10/2015"), Set.empty)
      val ma1T = Timetable(ma1Prak.id, ma1Entries, fd.parseLocalDate("26/10/2015"), Set.empty)

      val gens = scheduleService.gens
      val comp = scheduleService.generate(ap1T, Vector.empty, ap1G, ap1Plan, semester, Vector.empty)._1.elem
      val result = scheduleService.generate(ma1T, Vector.empty, ma1G, ma1Plan, semester, Vector(comp))

      result._2 shouldBe gens
      result._1.evaluate.value should be > gens
      result._1.evaluate.err should not be empty
    }
  }

  def emptyEval: Evaluation[Conflict, Int] = Evaluation.empty[Conflict, Int]
  def eval(l: List[Conflict]): Evaluation[Conflict, Int] = Evaluation.withError[Conflict, Int](l)

  def unfold[A, B](a: A)(f: A => Option[(B, A)]): Stream[B] = f(a) match {
    case Some((b, aa)) => Stream.cons(b, unfold(aa)(f))
    case None => Stream.empty
  }

  implicit class ScheduleGenUtils(s: ScheduleGen) {
    import utils.LwmDateTime.localDateTimeOrd

    def sorted = s.entries.sortBy(e => e.date.toLocalDateTime(e.start))
  }

  def toLocalDateTime(e: ScheduleEntryGen) = e.date.toLocalDateTime(e.start)

  def alph(amount: Int): Vector[String] = (unfold('A')(a => Option((a.toString, (a + 1).toChar))) take (amount % 27)).toVector

  def population(n: Int): Vector[UUID] = (Stream.continually(UUID.randomUUID()) take n).toVector

  val scheduleService = new ScheduleService(20, 100, 10)

  val semester = Semester("", "", LocalDate.now, LocalDate.now.plusWeeks(30), LocalDate.now.plusWeeks(4))
  val weeks = Weeks.weeksBetween(semester.start, semester.examStart)

  val ft = DateTimeFormat.forPattern("HH:mm:ss")
  val fd = DateTimeFormat.forPattern("dd/MM/yyyy")

  val ap1 = Course("ap1", "c1", "abbrev", User.randomUUID, 1)
  val ma1 = Course("ma1", "c2", "abbrev", User.randomUUID, 1)
  val degree = UUID.randomUUID
  val semester1 = UUID.randomUUID
  val ap1Prak = Labwork("ap1Prak", "desc1", semester1, ap1.id, degree)
  val ma1Prak = Labwork("ma1Prak", "desc2", semester1, ma1.id, degree)

  val labwork = labworks.take(1)
  val labId = labwork.head.id
  val timetable = populateTimetables(1, 6)(employees, labwork, List.empty).head.toUniqueEntity
  val plan = populateAssignmentPlans(1, 8)(labwork)(_ => 1).head.toUniqueEntity
  val groups = populateGroups(8)(labwork, students).map(_.toUniqueEntity).toVector
}*/
