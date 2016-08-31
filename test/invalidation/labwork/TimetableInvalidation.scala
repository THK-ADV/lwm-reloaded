package invalidation.labwork

import base.SesameDbSpec
import models.labwork.{Labwork, Timetable, TimetableEntry}
import models.users.User
import models.{Degree, Room}
import org.joda.time.{LocalDate, LocalTime}

import scala.util.Random._
import scala.util.Success

class TimetableInvalidation extends SesameDbSpec {

  "A Timetable invalidation" should {

    def tte: Stream[TimetableEntry] = Stream.continually(TimetableEntry(Set(User.randomUUID), Room.randomUUID, Degree.randomUUID, 1, LocalTime.now, LocalTime.now plusHours 2))

    def tt: Stream[Timetable] = Stream.continually(Timetable(Labwork.randomUUID, (tte take 20).toSet, LocalDate.now, Set()))

    "invalidate the timetable and subsequent timetable entries" in {
      import bindings.TimetableDescriptor

      val timetables = (tt take 100).toSet
      val toInvalidate = shuffle(timetables) take 30

      repo.addMany[Timetable](timetables)

      toInvalidate foreach (a => repo.invalidate[Timetable](Timetable.generateUri(a)))

      repo.getAll[Timetable] shouldBe Success(timetables diff toInvalidate)
      repo.deepGetAll[Timetable] map (_ map (_.id)) shouldBe Success(timetables map (_.id))
    }
  }

}
