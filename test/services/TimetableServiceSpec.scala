package services

import base.TestBaseDefinition
import models.users.{Student, Employee}
import models._
import models.schedule.{TimetableEntry, TimetableProtocol}
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import org.scalatest.WordSpec

class TimetableServiceSpec extends WordSpec with TestBaseDefinition {

  import ScheduleG.dateOrd

  val timetableService = new TimetableService

  "A TimetableService" should {

    "pass timetable entries when local and global blacklists are empty" in {
      val entries = (0 until 100).map {
        case n =>
          val date = DateTime.now.plusWeeks(n)
          TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, date, date, date, date)
      }.toSet

      val local = Set.empty[DateTime]
      val global = Set.empty[DateTime]

      val result = timetableService.applyBlacklist(entries, local, global)

      local.count(l => entries.exists(_.date == l)) shouldBe local.size
      global.count(g => entries.exists(_.date == g)) shouldBe global.size

      result.subsetOf(entries) shouldBe true
      result shouldBe entries
      result.forall(a => local.exists(_ == a)) shouldBe false
      result.forall(a => global.exists(_ == a)) shouldBe false
      result.map(_.date).toVector.sorted shouldBe sorted
    }

    "apply local and global blacklist dates on timetable entries" in {
      import scala.util.Random.shuffle

      val entries = (0 until 100).map {
        case n =>
          val date = DateTime.now.plusWeeks(n)
          TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, date, date, date, date)
      }.toSet

      val blacklist = entries.map(_.date)
      val local = shuffle(blacklist).take(10)
      val global = shuffle(blacklist).take(10)

      val result = timetableService.applyBlacklist(entries, local, global)

      local.count(l => entries.exists(_.date == l)) shouldBe local.size
      global.count(g => entries.exists(_.date == g)) shouldBe global.size

      result.subsetOf(entries) shouldBe true
      result.size should be < entries.size
      result.forall(a => local.exists(_ == a)) shouldBe false
      result.forall(a => global.exists(_ == a)) shouldBe false
      result.map(_.date).toVector.sorted shouldBe sorted
    }

    "extrapolate further entries based on frontend's timetable protocol template and assignment plan" in {
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
      val protocol = TimetableProtocol(Labwork.randomUUID, tEntries, fd.parseDateTime("19/10/2015"))
      val numberOfEntries = 7
      val aEntries = (0 until numberOfEntries).map(AssignmentEntry(_, Set.empty[EntryType])).toSet
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

      val result = timetableService.extrapolateEntries(protocol, plan, groups)

      result.labwork shouldBe protocol.labwork
      result.start shouldBe protocol.start
      result.buffer shouldBe protocol.buffer
      result.entries.size should be > protocol.entries.size
      result.entries.size shouldBe groups.size * plan.numberOfEntries
      result.entries.toVector.map(_.start).sorted shouldBe sorted
    }
  }
}
