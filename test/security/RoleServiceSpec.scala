package security

import java.util.UUID

import base.TestBaseDefinition
import org.scalatest.WordSpec
import services.RoleService

class RoleServiceSpec extends WordSpec with TestBaseDefinition {

  val sufficientPermissions = Set(Permission("view"), Permission("create"), Permission("delete"))
  val insufficientPermissions = Set(Permission("view"), Permission("delete"))

  val module1 = UUID.randomUUID()
  val module2 = UUID.randomUUID()
  val role1 = Role("testRole1", sufficientPermissions)
  val role2 = Role("testRole2", insufficientPermissions)

  val noneModule1Role1 = RefRole(None, role1)
  val noneModule1Role2 = RefRole(None, role2)

  val module1UserRole1 = RefRole(Some(module1), role1)
  val module1UserRole2 = RefRole(Some(module1), role2)
  val module2UserRole2 = RefRole(Some(module2), role2)

  def roleService = new RoleService()

  "A role service" should {

    "check refroles properly" in {
      val result1 = roleService.checkWith(Set(module1UserRole1))(Set(module1UserRole2))
      val result2 = roleService.checkWith(Set(module1UserRole1))(Set(module1UserRole1, module2UserRole2))
      val result3 = roleService.checkWith(Set(noneModule1Role1))(Set(module1UserRole1, noneModule1Role1, module2UserRole2))
      val result4 = roleService.checkWith(Set(module1UserRole1))(Set(noneModule1Role1, module2UserRole2))
      val result5 = roleService.checkWith(Set(module1UserRole1, module2UserRole2))(Set(module1UserRole1, module2UserRole2))
      val result6 = roleService.checkWith(Set(module1UserRole1, module2UserRole2))(Set(module1UserRole1))
      val result7 = roleService.checkWith(Set(noneModule1Role1))(Set(noneModule1Role1, noneModule1Role2))


      result1 shouldBe false
      result2 shouldBe true
      result3 shouldBe true
      result4 shouldBe false
      result5 shouldBe true
      result6 shouldBe false
      result7 shouldBe true
    }
  }

}
