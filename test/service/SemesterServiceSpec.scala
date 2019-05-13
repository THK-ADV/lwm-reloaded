package service

import base.DatabaseSpec
import play.api.inject.guice.GuiceableModule
import service.actor.NaturalDescribableYear

class SemesterServiceSpec extends DatabaseSpec {

  private val service = app.injector.instanceOf(classOf[SemesterService])

  "A SemesterServiceSpec" should {
    "create summer semester by year" in {
      val year = NaturalDescribableYear(2018)
      val summer = service.summerSemester(year)

      summer.label shouldBe "Sommersemester 2018"
      summer.abbreviation shouldBe "SoSe 18"
    }

    "create winter semester by year" in {
      val year = NaturalDescribableYear(2018)
      val winter = service.winterSemester(year)

      winter.label shouldBe "Wintersemester 2018/2019"
      winter.abbreviation shouldBe "WS 18/19"
    }
  }

  override protected def bindings: Seq[GuiceableModule] = Seq.empty
}
