package services

import java.util.UUID

import base.TestBaseDefinition
import models.{LabworkApplication, User}
import org.joda.time.DateTime
import org.scalatest.WordSpec
import utils.PreferenceSort._

import scala.util.Random

class GroupServiceSpec extends WordSpec with TestBaseDefinition {

  val labwork = UUID.randomUUID
  val users = ((0 until 100) map (_ => UUID.randomUUID)).toVector
  val apps = users.map(id => LabworkApplication(labwork, id, Set.empty, DateTime.now))

  "A GroupServiceSpec" should {
    "generate an alphabetical sequence of letters" in {
      val checkAgainst = 'A' to 'Z'
      val alphabetically = GroupService.alphabeticalOrdering(26)

      checkAgainst.forall(c => alphabetically.contains(c.toString)) shouldBe true
    }

    "sort a list of applicants for a given labwork" in {
      val users = Vector(
        User.randomUUID,
        User.randomUUID,
        User.randomUUID,
        User.randomUUID,
        User.randomUUID,
        User.randomUUID,
        User.randomUUID
      )

      val applications = Vector(
        LabworkApplication(labwork, users(0), Set(users(1)), DateTime.now),
        LabworkApplication(labwork, users(1), Set(users(0), users(3)), DateTime.now),
        LabworkApplication(labwork, users(2), Set(users(3)), DateTime.now),
        LabworkApplication(labwork, users(3), Set(users(2), users(1)), DateTime.now),
        LabworkApplication(labwork, users(4), Set(users(5)), DateTime.now),
        LabworkApplication(labwork, users(5), Set(users(4)), DateTime.now),
        LabworkApplication(labwork, users(6), Set.empty, DateTime.now)
      )

      val sorted = GroupService.sort(applications)
      val directlySorted = sort(Random.shuffle(applications).map(z => (z.applicant, z.friends)))

      sorted.last shouldBe directlySorted.last
    }

    "generate groups by given strategy" in {
      val count = CountGrouping("8")
      val range = RangeGrouping("13", "15")

      val groups = GroupService.groupApplicantsBy(count, apps, labwork)
      groups.size shouldBe count.value.toInt
      groups.flatMap(_.members).sorted shouldBe users.sorted

      val groups2 = GroupService.groupApplicantsBy(range, apps, labwork)
      groups2.size shouldBe Math.round(users.size.toFloat / range.max.toFloat)
      groups2.flatMap(_.members).sorted shouldBe users.sorted
    }

    "apply grouping strategies properly" in {
      val twentyfour = users.take(24)
      val twentysix = users.take(26)
      val twentyseven = users.take(27)
      val twentynine = users.take(29)
      val ten = users.take(10)

      val four = 4
      val shouldBeFour = CountGrouping(four.toString).apply(twentyfour)
      shouldBeFour.size shouldBe four
      shouldBeFour.forall(_.size == twentyfour.size / four) shouldBe true

      val shouldBeFour2 = CountGrouping(four.toString).apply(ten)
      shouldBeFour2.size shouldBe four
      shouldBeFour2.count(_.size == ten.size / four) shouldBe ten.size / four
      shouldBeFour2.count(_.size == 1 + (ten.size / four)) shouldBe ten.size / four

      val five = 5
      val shouldBeFive = CountGrouping(five.toString).apply(twentyfour)
      shouldBeFive.size shouldBe five
      shouldBeFive.count(_.size == five) shouldBe five - 1
      shouldBeFive.count(_.size == twentyfour.size % five) shouldBe 1

      val one = 1
      val shouldBeOne = CountGrouping(one.toString).apply(ten)
      shouldBeOne.size shouldBe one
      shouldBeOne.forall(_.size == ten.size) shouldBe true

      val two = 2
      val shouldBeTwo = CountGrouping(two.toString).apply(ten)
      shouldBeTwo.size shouldBe two
      shouldBeTwo.forall(_.size == ten.size / two) shouldBe true

      val three = 3
      val shouldBeThree = CountGrouping(three.toString).apply(ten)
      shouldBeThree.size shouldBe three
      shouldBeThree.count(_.size == three) shouldBe three - 1
      shouldBeThree.count(_.size == three + (ten.size % three)) shouldBe 1

      val ten2 = 10
      val shouldBeTen = CountGrouping(ten2.toString).apply(ten)
      shouldBeTen.size shouldBe ten2
      shouldBeTen.forall(_.size == shouldBeTen.size / ten2) shouldBe true

      val minEight = 8
      val maxTen = 10
      val eightToTen = RangeGrouping(minEight.toString, maxTen.toString)
      val r1 = eightToTen.apply(twentysix)
      val r2 = eightToTen.apply(twentyseven)
      val r3 = eightToTen.apply(twentynine)

      List(r1, r2, r3).forall(_.size == 3) shouldBe true
      r1.take(2).forall(_.size == minEight + 1) shouldBe true
      r1.last.size shouldBe minEight

      r2.forall(_.size == minEight + 1) shouldBe true

      r3.take(2).forall(_.size == minEight + 1 + 1) shouldBe true
      r3.last.size shouldBe minEight + 1

      val nine = 9
      val eightToNine = RangeGrouping(minEight.toString, nine.toString)
      val r4 = eightToNine.apply(twentynine)

      r4.take(2).forall(_.size == minEight + 1 + 1) shouldBe true
      r4.last.size shouldBe minEight + 1
    }
  }
}
