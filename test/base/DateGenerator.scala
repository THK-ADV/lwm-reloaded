package base

import org.joda.time.{DateTime, LocalDate, LocalTime}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.Suite

trait DateGenerator { this: Suite =>

  def dates: Gen[DateTime] = {
    val now = DateTime.now
    for {
      l <- Gen.choose(0, now.getMillis)
    } yield now.minus(l)
  }

  implicit val arbitraryDateTime: Arbitrary[DateTime] = Arbitrary(dates)

  implicit val arbitraryLocalTime: Arbitrary[LocalTime] = Arbitrary(dates.map(_.toLocalTime))

  implicit val arbitraryLocalDate: Arbitrary[LocalDate] = Arbitrary(dates.map(_.toLocalDate))
}
