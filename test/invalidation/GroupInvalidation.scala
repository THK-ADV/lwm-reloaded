package invalidation

import java.util.UUID

import base.SesameDbSpec
import models.{SesameGroup, SesameLabwork, User}

import scala.util.Random._
import scala.util.Success

class GroupInvalidation extends SesameDbSpec {

  "A Group invalidation" should {

    def people: Stream[UUID] = Stream.continually(User.randomUUID)
    def grps: Stream[SesameGroup] = Stream.continually(SesameGroup("Label", SesameLabwork.randomUUID, (people take 10).toSet))

    "invalidate the group" in {
      import bindings.GroupDescriptor

      val groups = (grps take 100).toSet
      val toInvalidate = shuffle(groups) take 30

      repo.addMany[SesameGroup](groups)

      toInvalidate foreach (a => repo.invalidate[SesameGroup](SesameGroup.generateUri(a)))

      repo.getAll[SesameGroup] shouldBe Success(groups diff toInvalidate)
      repo.deepGetAll[SesameGroup] map (_ map (_.id)) shouldBe Success(groups map (_.id))
    }
  }

}
