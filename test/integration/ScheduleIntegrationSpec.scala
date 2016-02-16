package integration

import java.util.UUID

import base.TestBaseDefinition
import models._
import models.schedule.{Timetable, TimetableProtocol, TimetableEntry}
import models.semester.{Blacklist, Semester}
import models.users.{Student, Employee}
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.scalatest.WordSpec
import services._
import store.SesameRepository

import scala.util.Success

class ScheduleIntegrationSpec extends WordSpec with TestBaseDefinition {

  val scheduleService = new ScheduleService
  val scheduleGenesisService = new ScheduleGenesisService(scheduleService)

  val repo = org.scalatest.mock.MockitoSugar.mock[SesameRepository]
  val blacklistService = new BlacklistService(repo)
  val timetableService = new TimetableService(blacklistService)

  val ft = DateTimeFormat.forPattern("dd/MM/yyyy HH:mm:ss")
  val fd = DateTimeFormat.forPattern("dd/MM/yyyy")

  "A ScheduleIntegrationSpec" should {

    "generate an collision free schedule in few generations even with one existing competitive schedule" in {
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
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("27/10/2015"), ft.parseDateTime("27/10/2015 08:00:00"), ft.parseDateTime("27/10/2015 09:00:00"), fd.parseDateTime("27/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("27/10/2015"), ft.parseDateTime("27/10/2015 09:00:00"), ft.parseDateTime("27/10/2015 10:00:00"), fd.parseDateTime("27/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("27/10/2015"), ft.parseDateTime("27/10/2015 10:00:00"), ft.parseDateTime("27/10/2015 11:00:00"), fd.parseDateTime("27/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("29/10/2015"), ft.parseDateTime("29/10/2015 14:00:00"), ft.parseDateTime("29/10/2015 15:00:00"), fd.parseDateTime("29/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("29/10/2015"), ft.parseDateTime("29/10/2015 14:00:00"), ft.parseDateTime("29/10/2015 15:00:00"), fd.parseDateTime("29/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("29/10/2015"), ft.parseDateTime("29/10/2015 15:00:00"), ft.parseDateTime("29/10/2015 16:00:00"), fd.parseDateTime("29/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("29/10/2015"), ft.parseDateTime("29/10/2015 15:00:00"), ft.parseDateTime("29/10/2015 16:00:00"), fd.parseDateTime("29/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("29/10/2015"), ft.parseDateTime("29/10/2015 16:00:00"), ft.parseDateTime("29/10/2015 17:00:00"), fd.parseDateTime("29/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("29/10/2015"), ft.parseDateTime("29/10/2015 16:00:00"), ft.parseDateTime("29/10/2015 17:00:00"), fd.parseDateTime("29/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 08:00:00"), ft.parseDateTime("30/10/2015 09:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 08:00:00"), ft.parseDateTime("30/10/2015 09:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 09:00:00"), ft.parseDateTime("30/10/2015 10:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 09:00:00"), ft.parseDateTime("30/10/2015 10:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 10:00:00"), ft.parseDateTime("30/10/2015 11:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 10:00:00"), ft.parseDateTime("30/10/2015 11:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 11:00:00"), ft.parseDateTime("30/10/2015 12:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 11:00:00"), ft.parseDateTime("30/10/2015 12:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 12:00:00"), ft.parseDateTime("30/10/2015 13:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 12:00:00"), ft.parseDateTime("30/10/2015 13:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 13:00:00"), ft.parseDateTime("30/10/2015 14:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 13:00:00"), ft.parseDateTime("30/10/2015 14:00:00"), fd.parseDateTime("30/10/2015"))
      )
      val ma1Entries = Set(
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("26/10/2015"), ft.parseDateTime("26/10/2015 08:00:00"), ft.parseDateTime("26/10/2015 11:00:00"), fd.parseDateTime("26/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("27/10/2015"), ft.parseDateTime("27/10/2015 08:00:00"), ft.parseDateTime("27/10/2015 11:00:00"), fd.parseDateTime("27/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("29/10/2015"), ft.parseDateTime("29/10/2015 08:00:00"), ft.parseDateTime("29/10/2015 11:00:00"), fd.parseDateTime("29/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("29/10/2015"), ft.parseDateTime("29/10/2015 11:00:00"), ft.parseDateTime("29/10/2015 14:00:00"), fd.parseDateTime("29/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 11:00:00"), ft.parseDateTime("30/10/2015 14:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 14:00:00"), ft.parseDateTime("30/10/2015 17:00:00"), fd.parseDateTime("30/10/2015"))
      )

      import scala.util.Random._
      val students = (0 until 300).map(_ => Student.randomUUID).toVector
      val ap1G = shuffle(students).take(280).grouped(10).map(s => Group("", ap1Prak.id, s.toSet, Group.randomUUID)).toSet
      val ma1G = shuffle(students).take(280).grouped(20).map(s => Group("", ma1Prak.id, s.toSet, Group.randomUUID)).toSet

      when(repo.get[Blacklist](anyObject(), anyObject())).thenReturn(Success(Set(Blacklist.empty)))

      val ap1Timetable = timetableService.extrapolateEntries(
        TimetableProtocol(ap1Prak.id, ap1Entries, DateTime.now),
        ap1Plan,
        ap1G
      )

      val ma1Extra = timetableService.extrapolateEntries(
        TimetableProtocol(ma1Prak.id, ma1Entries, DateTime.now),
        ma1Plan,
        ma1G
      )

      val comp = scheduleService.populate(1, ap1Timetable, ap1G)
      val result = scheduleGenesisService.generate(ma1Prak.id, ma1Extra, ma1G, ma1Plan, comp)
      val eval = scheduleService.evaluate(result._1.elem, ma1Plan.numberOfEntries, comp)

      println(s"gen ${result._2}")
      println(s"conflict size ${eval.conflicts.size}")
      println(s"conflict value ${eval.value}")
      eval.conflicts shouldBe empty
      eval.value shouldBe 0

      result._2 should be > 0
      result._1.evaluate.err shouldBe empty
      result._1.evaluate.value shouldBe 0
      result._1.elem.labwork shouldEqual ma1Prak.id
      result._1.elem.entries.groupBy(_.group) forall {
        case (_, ss) => ss.size == ma1Plan.numberOfEntries
      } shouldBe true
    }

    "generate yet another collision free schedule in few generations even with two existing competitive schedule" in {
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
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("27/10/2015"), ft.parseDateTime("27/10/2015 08:00:00"), ft.parseDateTime("27/10/2015 09:00:00"), fd.parseDateTime("27/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("27/10/2015"), ft.parseDateTime("27/10/2015 09:00:00"), ft.parseDateTime("27/10/2015 10:00:00"), fd.parseDateTime("27/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("27/10/2015"), ft.parseDateTime("27/10/2015 10:00:00"), ft.parseDateTime("27/10/2015 11:00:00"), fd.parseDateTime("27/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("29/10/2015"), ft.parseDateTime("29/10/2015 14:00:00"), ft.parseDateTime("29/10/2015 15:00:00"), fd.parseDateTime("29/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("29/10/2015"), ft.parseDateTime("29/10/2015 14:00:00"), ft.parseDateTime("29/10/2015 15:00:00"), fd.parseDateTime("29/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("29/10/2015"), ft.parseDateTime("29/10/2015 15:00:00"), ft.parseDateTime("29/10/2015 16:00:00"), fd.parseDateTime("29/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("29/10/2015"), ft.parseDateTime("29/10/2015 15:00:00"), ft.parseDateTime("29/10/2015 16:00:00"), fd.parseDateTime("29/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("29/10/2015"), ft.parseDateTime("29/10/2015 16:00:00"), ft.parseDateTime("29/10/2015 17:00:00"), fd.parseDateTime("29/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("29/10/2015"), ft.parseDateTime("29/10/2015 16:00:00"), ft.parseDateTime("29/10/2015 17:00:00"), fd.parseDateTime("29/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 08:00:00"), ft.parseDateTime("30/10/2015 09:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 08:00:00"), ft.parseDateTime("30/10/2015 09:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 09:00:00"), ft.parseDateTime("30/10/2015 10:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 09:00:00"), ft.parseDateTime("30/10/2015 10:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 10:00:00"), ft.parseDateTime("30/10/2015 11:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 10:00:00"), ft.parseDateTime("30/10/2015 11:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 11:00:00"), ft.parseDateTime("30/10/2015 12:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 11:00:00"), ft.parseDateTime("30/10/2015 12:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 12:00:00"), ft.parseDateTime("30/10/2015 13:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 12:00:00"), ft.parseDateTime("30/10/2015 13:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 13:00:00"), ft.parseDateTime("30/10/2015 14:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 13:00:00"), ft.parseDateTime("30/10/2015 14:00:00"), fd.parseDateTime("30/10/2015"))
      )
      val ma1Entries = Set(
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("26/10/2015"), ft.parseDateTime("26/10/2015 08:00:00"), ft.parseDateTime("26/10/2015 11:00:00"), fd.parseDateTime("26/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("27/10/2015"), ft.parseDateTime("27/10/2015 08:00:00"), ft.parseDateTime("27/10/2015 11:00:00"), fd.parseDateTime("27/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("29/10/2015"), ft.parseDateTime("29/10/2015 08:00:00"), ft.parseDateTime("29/10/2015 11:00:00"), fd.parseDateTime("29/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("29/10/2015"), ft.parseDateTime("29/10/2015 11:00:00"), ft.parseDateTime("29/10/2015 14:00:00"), fd.parseDateTime("29/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 11:00:00"), ft.parseDateTime("30/10/2015 14:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 14:00:00"), ft.parseDateTime("30/10/2015 17:00:00"), fd.parseDateTime("30/10/2015"))
      )
      val gdvkEntries = Set(
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 08:00:00"), ft.parseDateTime("30/10/2015 11:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 11:00:00"), ft.parseDateTime("30/10/2015 13:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 14:00:00"), ft.parseDateTime("30/10/2015 17:00:00"), fd.parseDateTime("30/10/2015"))
      )

      import scala.util.Random._
      val students = (0 until 300).map(_ => Student.randomUUID).toVector
      val ap1G = shuffle(students).take(280).grouped(10).map(s => Group("", ap1Prak.id, s.toSet, Group.randomUUID)).toSet
      val ma1G = shuffle(students).take(280).grouped(20).map(s => Group("", ma1Prak.id, s.toSet, Group.randomUUID)).toSet
      val gdvkG = shuffle(students).take(270).grouped(30).map(s => Group("", gdvkPrak.id, s.toSet, Group.randomUUID)).toSet

      when(repo.get[Blacklist](anyObject(), anyObject())).thenReturn(Success(Set(Blacklist.empty)))

      val ap1Timetable = timetableService.extrapolateEntries(
        TimetableProtocol(ap1Prak.id, ap1Entries, DateTime.now),
        ap1Plan,
        ap1G
      )
      val ma1Timetable = timetableService.extrapolateEntries(
        TimetableProtocol(ma1Prak.id, ma1Entries, DateTime.now),
        ma1Plan,
        ma1G
      )
      val gdvkTimetable = timetableService.extrapolateEntries(
        TimetableProtocol(gdvkPrak.id, gdvkEntries, DateTime.now),
        gdvkPlan,
        gdvkG
      )

      val compAp1 = scheduleService.populate(1, ap1Timetable, ap1G)
      val resultMa1 = scheduleGenesisService.generate(ma1Prak.id, ma1Timetable, ma1G, ma1Plan, compAp1)
      val evalMa1 = scheduleService.evaluate(resultMa1._1.elem, ma1Plan.numberOfEntries, compAp1)

      evalMa1 match {
        case e if e.conflicts.isEmpty =>
          val compAp1Ma1 = compAp1 ++ Vector(resultMa1._1.elem)
          val resultGdvk = scheduleGenesisService.generate(gdvkPrak.id, gdvkTimetable, gdvkG, gdvkPlan, compAp1Ma1)
          val evalGdvk = scheduleService.evaluate(resultGdvk._1.elem, gdvkPlan.numberOfEntries, compAp1Ma1)

          println(s"gen ${resultGdvk._2}")
          println(s"conflict size ${evalGdvk.conflicts.size}")
          println(s"conflict value ${evalGdvk.value}")
          evalGdvk.conflicts shouldBe empty
          evalGdvk.value shouldBe 0

          resultGdvk._2 should be > 0
          resultGdvk._1.evaluate.err shouldBe empty
          resultGdvk._1.evaluate.value shouldBe 0
          resultGdvk._1.elem.labwork shouldEqual gdvkPrak.id
          resultGdvk._1.elem.entries.groupBy(_.group) forall {
            case (_, ss) => ss.size == gdvkPlan.numberOfEntries
          } shouldBe true
        case _ => fail("ma1 failed")
      }
    }

    "generate n schedules with competitive ones" in {
      def gen(specs: Vector[(UUID, Timetable, Set[Group], AssignmentPlan)]): Vector[(ScheduleG, Evaluation)] = {
        def tryGen(id: UUID, t: Timetable, g: Set[Group], ap: AssignmentPlan, comp: Vector[ScheduleG]): (ScheduleG, Evaluation) = {
          val result = scheduleGenesisService.generate(id, t, g, ap, comp)
          val eval = scheduleService.evaluate(result._1.elem, ap.numberOfEntries, comp)

          if (eval.conflicts.isEmpty)
            (result._1.elem, eval)
          else {
            tryGen(id, t, g, ap, comp)
          }
        }

        specs.foldLeft((Vector.empty[ScheduleG], Vector.empty[(ScheduleG, Evaluation)])) {
          case ((comp, schedule), (id, t, g, ap)) =>
            val result = tryGen(id, t, g, ap, comp)

            (comp ++ Vector(result._1), Vector((result._1, result._2)))
        }._2
      }

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
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("27/10/2015"), ft.parseDateTime("27/10/2015 08:00:00"), ft.parseDateTime("27/10/2015 09:00:00"), fd.parseDateTime("27/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("27/10/2015"), ft.parseDateTime("27/10/2015 09:00:00"), ft.parseDateTime("27/10/2015 10:00:00"), fd.parseDateTime("27/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("27/10/2015"), ft.parseDateTime("27/10/2015 10:00:00"), ft.parseDateTime("27/10/2015 11:00:00"), fd.parseDateTime("27/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("29/10/2015"), ft.parseDateTime("29/10/2015 14:00:00"), ft.parseDateTime("29/10/2015 15:00:00"), fd.parseDateTime("29/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("29/10/2015"), ft.parseDateTime("29/10/2015 14:00:00"), ft.parseDateTime("29/10/2015 15:00:00"), fd.parseDateTime("29/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("29/10/2015"), ft.parseDateTime("29/10/2015 15:00:00"), ft.parseDateTime("29/10/2015 16:00:00"), fd.parseDateTime("29/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("29/10/2015"), ft.parseDateTime("29/10/2015 15:00:00"), ft.parseDateTime("29/10/2015 16:00:00"), fd.parseDateTime("29/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("29/10/2015"), ft.parseDateTime("29/10/2015 16:00:00"), ft.parseDateTime("29/10/2015 17:00:00"), fd.parseDateTime("29/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("29/10/2015"), ft.parseDateTime("29/10/2015 16:00:00"), ft.parseDateTime("29/10/2015 17:00:00"), fd.parseDateTime("29/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 08:00:00"), ft.parseDateTime("30/10/2015 09:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 08:00:00"), ft.parseDateTime("30/10/2015 09:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 09:00:00"), ft.parseDateTime("30/10/2015 10:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 09:00:00"), ft.parseDateTime("30/10/2015 10:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 10:00:00"), ft.parseDateTime("30/10/2015 11:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 10:00:00"), ft.parseDateTime("30/10/2015 11:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 11:00:00"), ft.parseDateTime("30/10/2015 12:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 11:00:00"), ft.parseDateTime("30/10/2015 12:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 12:00:00"), ft.parseDateTime("30/10/2015 13:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 12:00:00"), ft.parseDateTime("30/10/2015 13:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 13:00:00"), ft.parseDateTime("30/10/2015 14:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 13:00:00"), ft.parseDateTime("30/10/2015 14:00:00"), fd.parseDateTime("30/10/2015"))
      )
      val ma1Entries = Set(
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("26/10/2015"), ft.parseDateTime("26/10/2015 08:00:00"), ft.parseDateTime("26/10/2015 11:00:00"), fd.parseDateTime("26/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("27/10/2015"), ft.parseDateTime("27/10/2015 08:00:00"), ft.parseDateTime("27/10/2015 11:00:00"), fd.parseDateTime("27/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("29/10/2015"), ft.parseDateTime("29/10/2015 08:00:00"), ft.parseDateTime("29/10/2015 11:00:00"), fd.parseDateTime("29/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("29/10/2015"), ft.parseDateTime("29/10/2015 11:00:00"), ft.parseDateTime("29/10/2015 14:00:00"), fd.parseDateTime("29/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 11:00:00"), ft.parseDateTime("30/10/2015 14:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 14:00:00"), ft.parseDateTime("30/10/2015 17:00:00"), fd.parseDateTime("30/10/2015"))
      )
      val gdvkEntries = Set(
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 08:00:00"), ft.parseDateTime("30/10/2015 11:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 11:00:00"), ft.parseDateTime("30/10/2015 13:00:00"), fd.parseDateTime("30/10/2015")),
        TimetableEntry(Employee.randomUUID, Room.randomUUID, mi.id, fd.parseDateTime("30/10/2015"), ft.parseDateTime("30/10/2015 14:00:00"), ft.parseDateTime("30/10/2015 17:00:00"), fd.parseDateTime("30/10/2015"))
      )

      import scala.util.Random._
      val students = (0 until 300).map(_ => Student.randomUUID).toVector
      val ap1G = shuffle(students).take(280).grouped(10).map(s => Group("", ap1Prak.id, s.toSet, Group.randomUUID)).toSet
      val ma1G = shuffle(students).take(280).grouped(20).map(s => Group("", ma1Prak.id, s.toSet, Group.randomUUID)).toSet
      val gdvkG = shuffle(students).take(270).grouped(30).map(s => Group("", gdvkPrak.id, s.toSet, Group.randomUUID)).toSet

      when(repo.get[Blacklist](anyObject(), anyObject())).thenReturn(Success(Set(Blacklist.empty)))

      val ap1Timetable = timetableService.extrapolateEntries(
        TimetableProtocol(ap1Prak.id, ap1Entries, DateTime.now),
        ap1Plan,
        ap1G
      )
      val ma1Timetable = timetableService.extrapolateEntries(
        TimetableProtocol(ma1Prak.id, ma1Entries, DateTime.now),
        ma1Plan,
        ma1G
      )
      val gdvkTimetable = timetableService.extrapolateEntries(
        TimetableProtocol(gdvkPrak.id, gdvkEntries, DateTime.now),
        gdvkPlan,
        gdvkG
      )

      val result = gen(Vector(
        (ap1Prak.id, ap1Timetable, ap1G, ap1Plan),
        (ma1Prak.id, ma1Timetable, ma1G, ma1Plan),
        (gdvkPrak.id, gdvkTimetable, gdvkG, gdvkPlan)
      ))

      result.size shouldBe 1
      println(s"conflict size ${result.head._2.conflicts.size}")
      println(s"conflict value ${result.head._2.value}")
      result.head._2.conflicts shouldBe empty
      result.head._2.value shouldBe 0

      result.head._1.entries.groupBy(_.group) forall {
        case (_, ss) => ss.size == gdvkPlan.numberOfEntries
      } shouldBe true
    }
  }
}
