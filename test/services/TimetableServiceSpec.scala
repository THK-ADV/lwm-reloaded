package services

import base.TestBaseDefinition
import models.semester.Blacklist
import models.users.{Student, Employee}
import models._
import models.schedule.{TimetableEntry, TimetableProtocol}
import org.joda.time.format.DateTimeFormat
import org.scalatest.WordSpec
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.scalatest.mock.MockitoSugar.mock
import store.SesameRepository

import scala.util.Success

class TimetableServiceSpec extends WordSpec with TestBaseDefinition {

  import ScheduleG.dateOrd

  val repo = mock[SesameRepository]
  val blacklistService = new BlacklistService(repo)
  val timetableService = new TimetableService(blacklistService)

  val ft = DateTimeFormat.forPattern("dd/MM/yyyy HH:mm:ss")
  val fd = DateTimeFormat.forPattern("dd/MM/yyyy")

  val degree = Degree("degree", Degree.randomUUID)
  val tEntries = Set(
    TimetableEntry(Employee.randomUUID, Room.randomUUID, degree.id, fd.parseDateTime("19/10/2015"), ft.parseDateTime("19/10/2015 11:00:00"), ft.parseDateTime("19/10/2015 13:00:00"), fd.parseDateTime("19/10/2015")),
    TimetableEntry(Employee.randomUUID, Room.randomUUID, degree.id, fd.parseDateTime("19/10/2015"), ft.parseDateTime("19/10/2015 13:00:00"), ft.parseDateTime("19/10/2015 15:00:00"), fd.parseDateTime("19/10/2015")),
    TimetableEntry(Employee.randomUUID, Room.randomUUID, degree.id, fd.parseDateTime("19/10/2015"), ft.parseDateTime("19/10/2015 15:00:00"), ft.parseDateTime("19/10/2015 17:00:00"), fd.parseDateTime("19/10/2015")),
    TimetableEntry(Employee.randomUUID, Room.randomUUID, degree.id, fd.parseDateTime("19/10/2015"), ft.parseDateTime("19/10/2015 17:00:00"), ft.parseDateTime("19/10/2015 19:00:00"), fd.parseDateTime("19/10/2015")),
    TimetableEntry(Employee.randomUUID, Room.randomUUID, degree.id, fd.parseDateTime("23/10/2015"), ft.parseDateTime("23/10/2015 15:00:00"), ft.parseDateTime("23/10/2015 17:00:00"), fd.parseDateTime("23/10/2015"))
  )
  val profileWeek = (0 until 5).map(n => fd.parseDateTime("23/11/2015").plusDays(n)).toSet
  val christmas = (0 until 3 * 7).map(n => fd.parseDateTime("21/12/2015").plusDays(n)).toSet
  val globalBlacklist = Set(Blacklist(profileWeek, Blacklist.randomUUID), Blacklist(christmas, Blacklist.randomUUID))
  when(repo.get[Blacklist](anyObject(), anyObject())).thenReturn(Success(globalBlacklist))

  "A TimetableService" should {

    "extrapolate further entries based on frontend's timetable protocol template and assignment plan where some assignments takes more than one week with global blacklists applied" in {
      val protocol = TimetableProtocol(Labwork.randomUUID, tEntries, fd.parseDateTime("19/10/2015"), Blacklist.empty)
      val numberOfEntries = 7
      val aEntries = (0 until numberOfEntries).map {
        case e if e < 5 =>
          AssignmentEntry(e, Set.empty[EntryType])
        case e =>
          AssignmentEntry(e, Set.empty[EntryType], e - 3)
      }.toSet
      val plan = AssignmentPlan(numberOfEntries, aEntries)
      val members = (0 until 20).map(_ => Student.randomUUID).toSet
      val groups = Set(
        Group("A", protocol.labwork, members),
        Group("B", protocol.labwork, members),
        Group("C", protocol.labwork, members),
        Group("D", protocol.labwork, members),
        Group("E", protocol.labwork, members),
        Group("F", protocol.labwork, members)
      )

      val expectedStart = Vector(
        ft.parseDateTime("19/10/2015 11:00:00"),
        ft.parseDateTime("26/10/2015 13:00:00"),
        ft.parseDateTime("02/11/2015 15:00:00"),
        ft.parseDateTime("09/11/2015 17:00:00"),
        ft.parseDateTime("20/11/2015 15:00:00"),
        ft.parseDateTime("14/12/2015 13:00:00"),
        ft.parseDateTime("29/01/2016 15:00:00")
      )

      val result = timetableService.extrapolateEntries(protocol, plan, groups)

      result.labwork shouldBe protocol.labwork
      result.start shouldBe protocol.start
      result.entries.size should be > protocol.entries.size
      result.entries.size shouldBe groups.size * plan.numberOfEntries
      result.entries.toVector.map(_.start).sorted shouldBe sorted
      globalBlacklist.forall(a => a.dates.subsetOf(result.entries.map(_.date))) shouldBe false
      result.entries.toVector.map(_.start).sorted.grouped(groups.size).forall(a => expectedStart.count(b => a.head.isEqual(b)) == 1) shouldBe true
      result.entries.toVector.map(_.start).sorted.grouped(groups.size).foldLeft((true, expectedStart)) {
        case ((bool, vec), e) =>
          (bool && e.head.isEqual(vec.head), vec.tail)
      }._1 shouldBe true
      result.entries.toVector.map(_.start).sorted.map(_.toString(ft)).grouped(groups.size).foreach(println)
    }

    "extrapolate further entries based on frontend's timetable protocol template and assignment plan where each assignment takes 2 weeks with global blacklists applied" in {
      val protocol = TimetableProtocol(Labwork.randomUUID, tEntries, fd.parseDateTime("19/10/2015"), Blacklist.empty)
      val numberOfEntries = 5
      val aEntries = (0 until numberOfEntries).map(AssignmentEntry(_, Set.empty[EntryType], 2)).toSet
      val plan = AssignmentPlan(numberOfEntries, aEntries)
      val members = (0 until 20).map(_ => Student.randomUUID).toSet
      val groups = Set(
        Group("A", protocol.labwork, members),
        Group("B", protocol.labwork, members),
        Group("C", protocol.labwork, members),
        Group("D", protocol.labwork, members),
        Group("E", protocol.labwork, members),
        Group("F", protocol.labwork, members)
      )

      val expectedStart = Vector(
        ft.parseDateTime("19/10/2015 11:00:00"),
        ft.parseDateTime("02/11/2015 15:00:00"),
        ft.parseDateTime("20/11/2015 15:00:00"),
        ft.parseDateTime("14/12/2015 13:00:00"),
        ft.parseDateTime("18/01/2016 17:00:00")
      )

      val result = timetableService.extrapolateEntries(protocol, plan, groups)

      result.labwork shouldBe protocol.labwork
      result.start shouldBe protocol.start
      result.entries.size should be > protocol.entries.size
      result.entries.size shouldBe groups.size * plan.numberOfEntries
      result.entries.toVector.map(_.start).sorted shouldBe sorted
      globalBlacklist.forall(a => a.dates.subsetOf(result.entries.map(_.date))) shouldBe false
      result.entries.toVector.map(_.start).sorted.grouped(groups.size).foldLeft((true, expectedStart)) {
        case ((bool, vec), e) =>
          (bool && e.head.isEqual(vec.head), vec.tail)
      }._1 shouldBe true
      result.entries.toVector.map(_.start).sorted.map(_.toString(ft)).grouped(groups.size).foreach(println)
    }

    "extrapolate further entries based on frontend's timetable protocol template and assignment plan where some assignments takes more than one week with local and global blacklists applied" in {
      val localBlacklist = Blacklist(Set(
        ft.parseDateTime("30/10/2015 15:00:00"),
        ft.parseDateTime("06/11/2015 15:00:00"),
        ft.parseDateTime("30/11/2015 11:00:00"),
        ft.parseDateTime("30/11/2015 13:00:00"),
        ft.parseDateTime("30/11/2015 15:00:00"),
        ft.parseDateTime("30/11/2015 17:00:00")
      ), Blacklist.randomUUID)

      val protocol = TimetableProtocol(Labwork.randomUUID, tEntries, fd.parseDateTime("19/10/2015"), localBlacklist)
      val numberOfEntries = 7
      val aEntries = (0 until numberOfEntries).map {
        case e if e < 5 =>
          AssignmentEntry(e, Set.empty[EntryType])
        case e =>
          AssignmentEntry(e, Set.empty[EntryType], e - 3)
      }.toSet
      val plan = AssignmentPlan(numberOfEntries, aEntries)
      val members = (0 until 20).map(_ => Student.randomUUID).toSet
      val groups = Set(
        Group("A", protocol.labwork, members),
        Group("B", protocol.labwork, members),
        Group("C", protocol.labwork, members),
        Group("D", protocol.labwork, members),
        Group("E", protocol.labwork, members),
        Group("F", protocol.labwork, members)
      )

      val expectedStart = Vector(
        ft.parseDateTime("19/10/2015 11:00:00"),
        ft.parseDateTime("26/10/2015 13:00:00"),
        ft.parseDateTime("02/11/2015 17:00:00"),
        ft.parseDateTime("16/11/2015 11:00:00"),
        ft.parseDateTime("07/12/2015 11:00:00"),
        ft.parseDateTime("11/01/2016 15:00:00"),
        ft.parseDateTime("08/02/2016 11:00:00")
      )

      val result = timetableService.extrapolateEntries(protocol, plan, groups)

      result.labwork shouldBe protocol.labwork
      result.start shouldBe protocol.start
      result.entries.size should be > protocol.entries.size
      result.entries.size shouldBe groups.size * plan.numberOfEntries
      result.entries.toVector.map(_.start).sorted shouldBe sorted
      globalBlacklist.forall(a => a.dates.subsetOf(result.entries.map(_.date))) shouldBe false
      localBlacklist.dates.subsetOf(result.entries.map(_.start)) shouldBe false
      result.entries.toVector.map(_.start).sorted.grouped(groups.size).foldLeft((true, expectedStart)) {
        case ((bool, vec), e) =>
          (bool && e.head.isEqual(vec.head), vec.tail)
      }._1 shouldBe true
      result.entries.toVector.map(_.start).sorted.map(_.toString(ft)).grouped(groups.size).foreach(println)
    }
  }
}
