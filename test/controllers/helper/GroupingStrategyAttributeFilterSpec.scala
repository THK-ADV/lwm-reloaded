package controllers.helper

import base.TestBaseDefinition
import org.scalatest.WordSpec
import services.{CountGrouping, RangeGrouping}

class GroupingStrategyAttributeFilterSpec extends WordSpec with TestBaseDefinition with AttributeFilter with GroupingStrategyAttributeFilter {

  private val randoms = Map(
    "firstAttribute" -> Seq("firstValue"),
    "secondAttribute" -> Seq("secondValue"),
    "thirdAttribute" -> Seq("thirdValue")
  )

  "A GroupingStrategyAttributeFilterSpec" should {
    "extract count strategy" in {
      val strategy = extractGroupingStrategy(Map(countAttribute -> Seq("5")) ++ randoms)
      strategy.success.value shouldBe CountGrouping(5)
    }

    "fail extracting count strategy when input is not a number" in {
      val strategy = extractGroupingStrategy(Map(countAttribute -> Seq("five")) ++ randoms)
      strategy.failure.exception should have message "For input string: \"five\""
    }

    "fail extracting count strategy when input is lower than zero" in {
      val strategy = extractGroupingStrategy(Map(countAttribute -> Seq("0")) ++ randoms)
      strategy.failure.exception should have message "Predicate does not hold for 0"
    }

    "extract min/max strategy" in {
      val strategy = extractGroupingStrategy(Map(minAttribute -> Seq("2"), maxAttribute -> Seq("5")) ++ randoms)
      strategy.success.value shouldBe RangeGrouping(2, 5)
    }

    "fail extracting min/max strategy when inputs are not numbers" in {
      val strategy = extractGroupingStrategy(Map(minAttribute -> Seq("two"), maxAttribute -> Seq("5")) ++ randoms)
      strategy.failure.exception should have message "For input string: \"two\""
    }

    "fail extracting min/max strategy when max is lower than min" in {
      val strategy = extractGroupingStrategy(Map(minAttribute -> Seq("5"), maxAttribute -> Seq("2")) ++ randoms)
      strategy.failure.exception should have message "Predicate does not hold for 2"
    }

    "fail extracting min/max strategy when either min or max is passed but not both" in {
      val strategy = extractGroupingStrategy(Map(minAttribute -> Seq("5")) ++ randoms)
      strategy.failure.exception should have message s"grouping strategy should be either $countAttribute or $minAttribute and $maxAttribute"
    }

    "fail extracting when both count and min/max are passed" in {
      val strategy = extractGroupingStrategy(Map(minAttribute -> Seq("5"), countAttribute -> Seq("1")) ++ randoms)
      strategy.failure.exception should have message s"grouping strategy should be either $countAttribute or $minAttribute and $maxAttribute"
    }
  }
}
