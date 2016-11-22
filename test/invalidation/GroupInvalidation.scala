package invalidation

import java.util.UUID

import base.SesameDbSpec
import models.{Group, Labwork, User}

import scala.util.Random._
import scala.util.Success

class GroupInvalidation extends SesameDbSpec {

  "A Group invalidation" should {

    def people: Stream[UUID] = Stream.continually(User.randomUUID)
    def grps: Stream[Group] = Stream.continually(Group("Label", Labwork.randomUUID, (people take 10).toSet))

    "invalidate the group" in {
      import bindings.GroupDescriptor

      val groups = (grps take 100).toSet
      val toInvalidate = shuffle(groups) take 30

      repo.addMany[Group](groups)

      toInvalidate foreach (a => repo.invalidate[Group](Group.generateUri(a)))

      repo.getAll[Group] shouldBe Success(groups diff toInvalidate)
      repo.deepGetAll[Group] map (_ map (_.id)) shouldBe Success(groups map (_.id))
    }
  }

}
