package service

import java.util.UUID

import base.TestBaseDefinition
import org.scalatest.WordSpec

class GroupingStrategySpec extends WordSpec with TestBaseDefinition {

  import service.GroupingStrategy._

  "A GroupingStrategySpec" should {

    "group different strategies properly" in {
      val users = ((0 until 100) map (_ => UUID.randomUUID)).toVector

      val twentyfour = users.take(24)
      val twentysix = users.take(26)
      val twentyseven = users.take(27)
      val twentynine = users.take(29)
      val ten = users.take(10)

      val four = 4
      val shouldBeFour = Count(four).group(twentyfour)
      shouldBeFour.size shouldBe four
      shouldBeFour.forall(_.size == twentyfour.size / four) shouldBe true

      val shouldBeFour2 = Count(four).group(ten)
      shouldBeFour2.size shouldBe four
      shouldBeFour2.count(_.size == ten.size / four) shouldBe ten.size / four
      shouldBeFour2.count(_.size == 1 + (ten.size / four)) shouldBe ten.size / four

      val five = 5
      val shouldBeFive = Count(five).group(twentyfour)
      shouldBeFive.size shouldBe five
      shouldBeFive.count(_.size == five) shouldBe five - 1
      shouldBeFive.count(_.size == twentyfour.size % five) shouldBe 1

      val one = 1
      val shouldBeOne = Count(one).group(ten)
      shouldBeOne.size shouldBe one
      shouldBeOne.forall(_.size == ten.size) shouldBe true

      val two = 2
      val shouldBeTwo = Count(two).group(ten)
      shouldBeTwo.size shouldBe two
      shouldBeTwo.forall(_.size == ten.size / two) shouldBe true

      val three = 3
      val shouldBeThree = Count(three).group(ten)
      shouldBeThree.size shouldBe three
      shouldBeThree.count(_.size == three) shouldBe three - 1
      shouldBeThree.count(_.size == three + (ten.size % three)) shouldBe 1

      val ten2 = 10
      val shouldBeTen = Count(ten2).group(ten)
      shouldBeTen.size shouldBe ten2
      shouldBeTen.forall(_.size == shouldBeTen.size / ten2) shouldBe true

      val minEight = 8
      val maxTen = 10
      val eightToTen = Range(minEight, maxTen)
      val r1 = eightToTen.group(twentysix)
      val r2 = eightToTen.group(twentyseven)
      val r3 = eightToTen.group(twentynine)

      List(r1, r2, r3).forall(_.size == 3) shouldBe true
      r1.take(2).forall(_.size == minEight + 1) shouldBe true
      r1.last.size shouldBe minEight

      r2.forall(_.size == minEight + 1) shouldBe true

      r3.take(2).forall(_.size == minEight + 1 + 1) shouldBe true
      r3.last.size shouldBe minEight + 1

      val nine = 9
      val eightToNine = Range(minEight, nine)
      val r4 = eightToNine.group(twentynine)

      r4.take(2).forall(_.size == minEight + 1 + 1) shouldBe true
      r4.last.size shouldBe minEight + 1
    }
  }
}
