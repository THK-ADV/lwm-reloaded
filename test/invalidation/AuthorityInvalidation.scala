package invalidation

import java.util.UUID

import base.SesameDbSpec
import models.SesameAuthority

import scala.util.Random._
import scala.util.Success

class AuthorityInvalidation extends SesameDbSpec {

  "An Authority invalidation" should {

    def auths: Stream[SesameAuthority] = {
      import scala.util.Random.nextBoolean

      val optCourse = if (nextBoolean) Some(UUID.randomUUID) else None
      Stream.continually(SesameAuthority(UUID.randomUUID, UUID.randomUUID, optCourse))
    }

    "invalidate the authority" in {
      import bindings.AuthorityDescriptor

      val authorities = (auths take 100).toSet
      val toInvalidate = shuffle(authorities) take 30

      repo.addMany[SesameAuthority](authorities)

      toInvalidate foreach (a => repo.invalidate[SesameAuthority](SesameAuthority.generateUri(a)))

      repo.getAll[SesameAuthority] shouldBe Success(authorities diff toInvalidate)

      repo.deepGetAll[SesameAuthority] map (_ map (_.id)) shouldBe Success(authorities map (_.id))
    }
  }

}
