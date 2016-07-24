package invalidation.security

import base.SesameDbSpec
import models.Course
import models.security.{RefRole, Role}

import scala.util.Random._
import scala.util.Success

/**
  * Created by robert on 23/07/16.
  */
class RefRoleInvalidation extends SesameDbSpec {

  "A RefRole invalidation" should {

    def refs: Stream[RefRole] = Stream.continually(RefRole(Some(Course.randomUUID), Role.randomUUID))

    "invalidate the ref role" in {
      import bindings.RefRoleDescriptor

      val refroles = (refs take 100).toSet
      val toInvalidate = shuffle(refroles) take 30

      repo.addMany[RefRole](refroles)

      toInvalidate foreach (a => repo.invalidate[RefRole](RefRole.generateUri(a)))

      repo.getAll[RefRole] shouldBe Success(refroles diff toInvalidate)
      repo.deepGetAll[RefRole] map (_ map (_.id)) shouldBe Success(refroles map (_.id))
    }
  }

}
