package invalidation.security

import base.SesameDbSpec
import models.security.Authority
import models.users.User

import scala.util.Random._
import scala.util.Success

class AuthorityInvalidation extends SesameDbSpec {

  "An Authority invalidation" should {

    def auths: Stream[Authority] = Stream.continually(Authority(User.randomUUID, Set()))

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
