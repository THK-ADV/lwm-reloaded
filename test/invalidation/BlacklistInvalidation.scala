package invalidation

import base.SesameDbSpec
import models.SesameBlacklist
import org.joda.time.DateTime

import scala.util.Random._
import scala.util.{Failure, Success}

class BlacklistInvalidation extends SesameDbSpec {

  "A Blacklist invalidation" should {

    def dates: Stream[DateTime] = Stream.continually(DateTime.now plusHours nextInt(20))
    def lists: Stream[SesameBlacklist] = Stream.continually(SesameBlacklist(s"Label$nextInt", (dates take 10).toSet))

    "invalidate the blacklist" in {
      import bindings.BlacklistDescriptor

      val blacklists = (lists take 100).toSet
      val toInvalidate = shuffle(blacklists) take 30

      repo.addMany[SesameBlacklist](blacklists)

      toInvalidate foreach (a => repo.invalidate[SesameBlacklist](SesameBlacklist.generateUri(a)))

      repo.getAll[SesameBlacklist] match {
        case Success(set) =>
          val valid = blacklists diff toInvalidate
          set.toVector.sortBy(_.id) zip valid.toVector.sortBy(_.id) foreach {
            case ((res, exp)) =>
              res.label shouldBe exp.label
              res.invalidated shouldBe exp.invalidated
              res.id shouldBe exp.id
              res.dates forall { date =>
                exp.dates exists (_ isEqual date)
              } shouldBe true
          }
        case Failure(e) => fail("no")
      }
      repo.deepGetAll[SesameBlacklist] map (_ map (_.id)) shouldBe Success(blacklists map (_.id))
    }
  }

}
