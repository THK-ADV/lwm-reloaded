package services

/*import akka.util.Timeout
import base.TestBaseDefinition
import models.Blacklist
import org.joda.time.{DateTime, LocalDate}
import org.scalatest.WordSpec
import services.blacklist.BlacklistServiceImpl

import scala.concurrent.Await
import scala.concurrent.duration._

final class BlacklistServiceSpec extends WordSpec with TestBaseDefinition {
  import utils.LwmDateTime._

  "A BlacklistServiceSpec" should {

    "fetch blacklists from an external api" in {
      val blacklistService = new BlacklistServiceImpl()
      val timeout = Timeout(5.seconds)
      val year = DateTime.now.getYear

      val result = Await.result(blacklistService.fetchLegalHolidays(year), timeout.duration)

      result should not be empty

      result.foreach { bl =>
        bl.date.localDate.getYear shouldBe year.toInt
        bl.start.localTime shouldBe Blacklist.startOfDay
        bl.end.localTime shouldBe Blacklist.endOfDay
        bl.global shouldBe true
      }

      result.map(_.date.localDate) should contain(LocalDate.now.withDayOfYear(1))
    }
  }
}*/
