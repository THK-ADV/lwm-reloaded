package invalidation.labwork

import java.util.UUID

import base.SesameDbSpec
import models.labwork.{Labwork, LabworkApplication}
import models.users.User

import scala.util.Random._
import scala.util.{Failure, Success}


class LabworkApplicationInvalidation extends SesameDbSpec {

  "A LabworkApplication invalidation" should {

    def people: Stream[UUID] = Stream.continually(User.randomUUID)
    def labapp: Stream[LabworkApplication] = Stream.continually(LabworkApplication(Labwork.randomUUID, User.randomUUID, (people take 10).toSet))

    "invalidate the labwork application" in {
      import bindings.LabworkApplicationDescriptor

      val apps = (labapp take 100).toSet
      val toInvalidate = shuffle(apps) take 30

      repo.addMany[LabworkApplication](apps)

      toInvalidate foreach (a => repo.invalidate[LabworkApplication](LabworkApplication.generateUri(a)))

      repo.getAll[LabworkApplication] match {
        case Success(set) =>
          set.toVector.sortBy(_.applicant) shouldBe (apps diff toInvalidate).toVector.sortBy(_.applicant)
        case Failure(e) => fail("no")
      }
      repo.deepGetAll[LabworkApplication] map (_ map (_.id)) shouldBe Success(apps map (_.id))
    }
  }
}
