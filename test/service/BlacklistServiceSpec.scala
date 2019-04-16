package service

import base.TestBaseDefinition
import models.Blacklist
import org.joda.time.DateTime
import org.scalatest.WordSpec
import org.scalatest.time.{Seconds, Span}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import service.blacklist.BlacklistApiService

final class BlacklistServiceSpec extends WordSpec with TestBaseDefinition with GuiceOneAppPerSuite {

  import scala.concurrent.ExecutionContext.Implicits.global

  val blacklistService = app.injector.instanceOf(classOf[BlacklistApiService])

  "A BlacklistServiceSpec" should {

    "fetch blacklists from an external api" in {
      import utils.date.DateTimeOps.{SqlDateConverter, SqlTimeConverter}
      val year = DateTime.now.getYear

      val future = blacklistService.fetchLegalHolidays(year)
      whenReady(future, timeout(Span(5, Seconds))) { result =>
        result should not be empty

        result.foreach { bl =>
          bl.date.localDate.getYear shouldBe year.toInt
          bl.start.localTime shouldBe Blacklist.startOfDay
          bl.end.localTime shouldBe Blacklist.endOfDay
          bl.global shouldBe true
        }
      }
    }
  }
}
