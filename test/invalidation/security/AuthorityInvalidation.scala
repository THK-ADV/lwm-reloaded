package invalidation.security

import java.util.UUID

import base.SesameDbSpec
import models.security.Authority

import scala.util.Random._
import scala.util.Success

class AuthorityInvalidation extends SesameDbSpec {

  "An Authority invalidation" should {

    def auths: Stream[Authority] = {
      import scala.util.Random.nextBoolean

      val optCourse = if (nextBoolean) Some(UUID.randomUUID) else None
      Stream.continually(Authority(UUID.randomUUID, UUID.randomUUID, optCourse))
    }

    "invalidate the authority" in {
      import bindings.AuthorityDescriptor

      val authorities = (auths take 100).toSet
      val toInvalidate = shuffle(authorities) take 30

      repo.addMany[Authority](authorities)

      toInvalidate foreach (a => repo.invalidate[Authority](Authority.generateUri(a)))

      repo.getAll[Authority] shouldBe Success(authorities diff toInvalidate)

      repo.deepGetAll[Authority] map (_ map (_.id)) shouldBe Success(authorities map (_.id))
    }
  }

}
