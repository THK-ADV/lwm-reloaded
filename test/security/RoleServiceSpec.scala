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

  val requiredModule1Role = RefRole(Some(module1), role1)
  val requiredNoneModuleRole = RefRole(None, role1)

  val noneModuleUserRole = RefRole(None, role1)
  val module1UserRoleValid = RefRole(Some(module1), role1)
  val module1UserRoleInvalid = RefRole(Some(module1), role2)
  val module2UserRole = RefRole(Some(module2), role2)

  def roleService = new RoleService()

  "A role service" should {

    "check refroles properly" in {
      val result1 = roleService.checkWith(Set(requiredModule1Role))(Set(module1UserRoleInvalid))
      val result2 = roleService.checkWith(Set(requiredModule1Role))(Set(module1UserRoleValid, module2UserRole))
      val result3 = roleService.checkWith(Set(requiredNoneModuleRole))(Set(module1UserRoleValid, noneModuleUserRole, module2UserRole))
      val result4 = roleService.checkWith(Set(requiredModule1Role))(Set(noneModuleUserRole, module2UserRole))


      result1 shouldBe false
      result2 shouldBe true
      result3 shouldBe true
      result4 shouldBe false
    }
  }

}
