package services

import base.TestBaseDefinition
import models.semester.Blacklist
import models.{Degree, Room}
import models.schedule.TimetableEntry
import models.users.Employee
import org.joda.time.DateTime
import org.scalatest.WordSpec
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.scalatest.mock.MockitoSugar.mock
import store.SesameRepository
import org.w3.banana.sesame.SesameModule

import scala.util.Success

class BlacklistServiceSpec extends WordSpec with TestBaseDefinition {

  import ScheduleG.dateOrd

  val repo = mock[SesameRepository]
  val blacklistService = new BlacklistService(repo)

  "A BlacklistServiceSpec" should {

    "pass timetable entries when local and global blacklists are empty" in {
      val global = Set(Blacklist.empty)
      when(repo.get[Blacklist](anyObject(), anyObject())).thenReturn(Success(global))

      val entries = (0 until 100).map {
        case n =>
          val date = DateTime.now.plusWeeks(n)
          TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, date, date, date, date)
      }.toSet

      val local = Blacklist.empty
      val result = blacklistService.applyBlacklist(entries, local)

      local.dates.count(l => entries.exists(_.start == l)) shouldBe local.dates.size
      global.head.dates.count(g => entries.exists(_.date == g)) shouldBe global.head.dates.size

      result.map(_.date).intersect(global.head.dates) shouldBe empty
      result.map(_.start).intersect(local.dates) shouldBe empty
      result.subsetOf(entries) shouldBe true

      result shouldBe entries
      result.size shouldBe entries.size
      result.forall(a => local.dates.exists(_ == a)) shouldBe false
      result.forall(a => global.head.dates.exists(_ == a)) shouldBe false
      result.map(_.date).toVector.sorted shouldBe sorted
    }

    "apply local and global blacklist dates on timetable entries" in {
      import scala.util.Random._

      val entries = (0 until 100).map {
        case n =>
          val date = DateTime.now.plusWeeks(n)
          TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, date, date.withHourOfDay(nextInt(24)), date.withHourOfDay(nextInt(24)), date)
      }.toSet
      val global1 = Blacklist(entries.slice(0, 10).map(_.date), Blacklist.randomUUID)
      val global2 = Blacklist(entries.slice(10, 20).map(_.date), Blacklist.randomUUID)
      when(repo.get[Blacklist](anyObject(), anyObject())).thenReturn(Success(Set(global1, global2)))

      val local = Blacklist(entries.slice(20, 30).map(_.start), Blacklist.randomUUID)
      val result = blacklistService.applyBlacklist(entries, local)

      local.dates.count(l => entries.exists(_.start == l)) shouldBe local.dates.size
      global1.dates.count(g => entries.exists(_.date == g)) shouldBe global1.dates.size
      global2.dates.count(g => entries.exists(_.date == g)) shouldBe global2.dates.size

      result.map(_.date).intersect(global1.dates) shouldBe empty
      result.map(_.date).intersect(global2.dates) shouldBe empty
      result.map(_.start).intersect(local.dates) shouldBe empty
      result.subsetOf(entries) shouldBe true

      result.size should be < entries.size
      result.size shouldBe entries.size - local.dates.size - global1.dates.size - global2.dates.size
      result.forall(a => local.dates.exists(_ == a)) shouldBe false
      result.forall(a => global1.dates.exists(_ == a)) shouldBe false
      result.forall(a => global2.dates.exists(_ == a)) shouldBe false
      result.map(_.date).toVector.sorted shouldBe sorted
    }
  }
}
