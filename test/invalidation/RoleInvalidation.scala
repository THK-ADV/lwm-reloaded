package invalidation

import base.SesameDbSpec
import models.{Permission, SesameRole$}

import scala.util.Random._
import scala.util.Success

class RoleInvalidation extends SesameDbSpec {


  "A Role invalidation" should {

    def perms: Stream[Permission] = Stream.continually(Permission(nextString(5)))
    def rls: Stream[SesameRole] = Stream.continually(SesameRole("Role", (perms take 20).toSet))

    "invalidate the role" in {
      import bindings.RoleDescriptor

      val roles = (rls take 100).toSet
      val toInvalidate = shuffle(roles) take 30

      repo.addMany[SesameRole](roles)

      toInvalidate foreach (a => repo.invalidate[SesameRole](SesameRole.generateUri(a)))

      repo.getAll[SesameRole] shouldBe Success(roles diff toInvalidate)
      repo.deepGetAll[SesameRole] map (_ map (_.id)) shouldBe Success(roles map (_.id))
    }
  }

}
