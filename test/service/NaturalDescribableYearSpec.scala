package service

import base.TestBaseDefinition
import org.joda.time.LocalDate
import org.scalatest.{OptionValues, WordSpec}

class NaturalDescribableYearSpec extends WordSpec with TestBaseDefinition with OptionValues {

  import service.NaturalDescribableYear._

  "A NaturalDescribableYearSpec" should {
    "build a year with int value" in {
      NaturalDescribableYear("2018").value shouldBe Year(2018)
      NaturalDescribableYear(2018) shouldBe Year(2018)
    }

    "fail building if input is invalid" in {
      NaturalDescribableYear("invalid") shouldBe empty
      NaturalDescribableYear.parse("invalid") shouldBe empty
    }

    "parse current and next year from string" in {
      val current = NaturalDescribableYear.parse("current")
      current.value shouldBe Current
      current.value.year shouldBe LocalDate.now.getYear

      val next = NaturalDescribableYear.parse("next")
      next.value shouldBe Next
      next.value.year shouldBe LocalDate.now.getYear + 1
    }

    "format years" in {
      val year = NaturalDescribableYear(2018)
      year.short shouldBe "18"
      year.long shouldBe "2018"

      val next = NaturalDescribableYear.parse("next")
      val jodaNext = LocalDate.now.plusYears(1)
      next.value.short shouldBe jodaNext.toString("yy")
      next.value.long shouldBe jodaNext.toString("yyyy")
    }
  }
}
