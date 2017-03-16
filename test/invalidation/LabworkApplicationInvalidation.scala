package invalidation

import java.util.UUID

import base.SesameDbSpec
import models.{SesameLabwork, SesameLabworkApplication, User}

import scala.util.Random._
import scala.util.{Failure, Success}

class LabworkApplicationInvalidation extends SesameDbSpec {

  "A LabworkApplication invalidation" should {

    def people: Stream[UUID] = Stream.continually(User.randomUUID)
    def labapp: Stream[SesameLabworkApplication] = Stream.continually(SesameLabworkApplication(SesameLabwork.randomUUID, User.randomUUID, (people take 10).toSet))

    "invalidate the labwork application" in {
      import bindings.LabworkApplicationDescriptor

      val apps = (labapp take 100).toSet
      val toInvalidate = shuffle(apps) take 30

      repo.addMany[SesameLabworkApplication](apps)

      toInvalidate foreach (a => repo.invalidate[SesameLabworkApplication](SesameLabworkApplication.generateUri(a)))

      repo.getAll[SesameLabworkApplication] match {
        case Success(set) =>
          set.toVector.sortBy(_.applicant) shouldBe (apps diff toInvalidate).toVector.sortBy(_.applicant)
        case Failure(e) => fail("no")
      }
      repo.deepGetAll[SesameLabworkApplication] map (_ map (_.id)) shouldBe Success(apps map (_.id))
    }
  }
}
