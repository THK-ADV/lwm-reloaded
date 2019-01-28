package controllers.helper

import base.TestBaseDefinition
import org.joda.time.DateTime
import org.scalatest.WordSpec
import utils.LwmDateTime.DateTimeConverter

final class AttributeFilterSpec extends WordSpec with TestBaseDefinition with AttributeFilter {

  private val attributes = Map(
    "firstAttribute" -> Seq("firstValue"),
    "secondAttribute" -> Seq("secondValue"),
    "thirdAttribute" -> Seq("thirdValue")
  )

  "A AttributeFilterSpec " should {

    "extract default attributes when each is given" in {
      val queryString = Map(
        atomicAttribute -> Seq(false.toString),
        validAttribute -> Seq(true.toString),
        lastModifiedAttribute -> Seq(DateTime.now.timestamp.getTime.toString)
      )

      val (attr, defaults) = extractAttributes(queryString ++ attributes)
      val defaultAttributes = DefaultAttributes(
        queryString(atomicAttribute).head.toBoolean,
        queryString(validAttribute).head.toBoolean,
        queryString(lastModifiedAttribute).headOption
      )

      defaults shouldBe defaultAttributes
      attr shouldBe attributes
    }

    "extract default attributes when some are given" in {
      val qs1 = Map(atomicAttribute -> Seq(false.toString))
      val qs2 = Map(validAttribute -> Seq(false.toString))
      val qs3 = Map(
        atomicAttribute -> Seq(false.toString),
        lastModifiedAttribute -> Seq(DateTime.now.timestamp.getTime.toString)
      )

      val (attr1, defaults1) = extractAttributes(qs1 ++ attributes)
      val (attr2, defaults2) = extractAttributes(qs2 ++ attributes)
      val (attr3, defaults3) = extractAttributes(qs3 ++ attributes)

      val defaultAttributes1 = DefaultAttributes(qs1(atomicAttribute).head.toBoolean)
      val defaultAttributes2 = DefaultAttributes(valid = qs2(validAttribute).head.toBoolean)
      val defaultAttributes3 = DefaultAttributes(
        atomic = qs3(atomicAttribute).head.toBoolean,
        lastModified = qs3(lastModifiedAttribute).headOption
      )

      defaults1 shouldBe defaultAttributes1
      defaults2 shouldBe defaultAttributes2
      defaults3 shouldBe defaultAttributes3
      attr1 shouldBe attributes
      attr2 shouldBe attributes
      attr3 shouldBe attributes
    }

    "extract default attributes by default" in {
      val (attr, defaults) = extractAttributes(attributes)
      val defaultAttributes = DefaultAttributes()

      defaults shouldBe defaultAttributes
      attr shouldBe attributes
    }
  }
}
