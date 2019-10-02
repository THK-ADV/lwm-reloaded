package service

import base.{DateGenerator, TestBaseDefinition}
import database.BlacklistDb
import org.joda.time.LocalDate
import org.scalatest.WordSpec

final class BlacklistServiceSpec extends WordSpec with TestBaseDefinition with DateGenerator {

  import service.BlacklistService._

  "A BlacklistServiceSpec" should {
    "create blacklists by a range of dates" in {
      def create(d: LocalDate) = BlacklistDb.entireDay("test", d, global = true)

      def normalize(b: BlacklistDb) = (b.label, b.date, b.start, b.end, b.global)

      val start = localDate(2019, 1, 28)
      val end = localDate(2019, 2, 5)

      val result = fromRange("test", start, end)
      val dest = List(
        create(localDate(2019, 1, 28)),
        create(localDate(2019, 1, 29)),
        create(localDate(2019, 1, 30)),
        create(localDate(2019, 1, 31)),
        create(localDate(2019, 2, 1)),
        create(localDate(2019, 2, 2)),
        create(localDate(2019, 2, 3)),
        create(localDate(2019, 2, 4)),
        create(localDate(2019, 2, 5)),
      )

      result.map(normalize) shouldBe dest.map(normalize)
    }
  }
}