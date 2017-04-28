package invalidation

import base.SesameDbSpec
import models._
import org.joda.time.{LocalDate, LocalTime}

import scala.util.Random._
import scala.util.Success

class TimetableInvalidation extends SesameDbSpec {

  "A Timetable invalidation" should {

    def tte: Stream[SesameTimetableEntry] = Stream.continually(SesameTimetableEntry(Set(User.randomUUID), SesameRoom.randomUUID, 1, LocalTime.now, LocalTime.now plusHours 2))

    def tt: Stream[SesameTimetable] = Stream.continually(SesameTimetable(SesameLabwork.randomUUID, (tte take 20).toSet, LocalDate.now, Set()))

    "invalidate the timetable and subsequent timetable entries" in {
      import bindings.TimetableDescriptor

      val timetables = (tt take 100).toSet
      val toInvalidate = shuffle(timetables) take 30

      repo.addMany[SesameTimetable](timetables)

      toInvalidate foreach (a => repo.invalidate[SesameTimetable](SesameTimetable.generateUri(a)))

      repo.getAll[SesameTimetable] shouldBe Success(timetables diff toInvalidate)
      repo.deepGetAll[SesameTimetable] map (_ map (_.id)) shouldBe Success(timetables map (_.id))
    }
  }

}
