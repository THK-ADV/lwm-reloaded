package services

import java.util.UUID

import base.TestBaseDefinition
import org.scalatest.WordSpec

final class GroupService2Spec extends WordSpec with TestBaseDefinition {

  private def people(amount: Int) = (0 until amount).map(_ => UUID.randomUUID).toVector

  "A GroupService2Spec" should {

    "apply grouping strategies properly" in {
      val twentyfour = people(24)
      val twentysix = people(26)
      val twentyseven = people(27)
      val twentynine = people(29)

      val four = 4
      CountGrouping(four.toString).apply(twentyfour).size shouldBe twentyfour.size / four

      val five = 5
      val countFive = CountGrouping(five.toString).apply(twentyfour)
      countFive.size shouldBe five
      countFive.take(five - 1).forall(_.size == five) shouldBe true
      countFive.last.size shouldBe five - 1

      val eight = 8
      val ten = 10
      val eightToTen = RangeGrouping(eight.toString, ten.toString)
      val r1 = eightToTen.apply(twentysix)
      val r2 = eightToTen.apply(twentyseven)
      val r3 = eightToTen.apply(twentynine)

      List(r1, r2, r3).forall(_.size == 3) shouldBe true
      r1.take(2).forall(_.size == eight + 1) shouldBe true
      r1.last.size shouldBe eight

      r2.forall(_.size == eight + 1) shouldBe true

      r3.take(2).forall(_.size == eight + 1 + 1) shouldBe true
      r3.last.size shouldBe eight + 1

      val nine = 9
      val eightToNine = RangeGrouping(eight.toString, nine.toString)
      val r4 = eightToNine.apply(twentynine)

      r4.take(2).forall(_.size == eight + 1 + 1) shouldBe true
      r4.last.size shouldBe eight + 1
    }
  }
}
