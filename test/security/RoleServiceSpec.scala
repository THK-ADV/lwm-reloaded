package security

import java.util.UUID

import base.TestBaseDefinition
import models.security._
import models.users.Student
import org.scalatest.WordSpec
import org.w3.banana.sesame.SesameModule
import services.RoleService
import store.bind.Bindings
import store.{Namespace, SesameRepository}

class RoleServiceSpec extends WordSpec with TestBaseDefinition with SesameModule {

  val sufficientPermissions = Set(Permission("view"), Permission("create"), Permission("delete"))
  val insufficientPermissions = Set(Permission("view"), Permission("delete"))

  val module1 = UUID.randomUUID()
  val module2 = UUID.randomUUID()
  val role1 = Role("testRole1", sufficientPermissions)
  val role2 = Role("testRole2", insufficientPermissions)
  val role3 = Roles.admin

  val noneModule1Role1 = RefRole(None, role1)
  val noneModule1Role2 = RefRole(None, role2)

  val module1UserRole1 = RefRole(Some(module1), role1)
  val module1UserRole2 = RefRole(Some(module1), role2)
  val module2UserRole2 = RefRole(Some(module2), role2)
  val adminRole = RefRole(None, Roles.admin)

  def unbox(r: RefRole): (Option[UUID], Set[Permission]) = (r.module, r.role.permissions)

  val ns = Namespace("http://lwm.gm.fh-koeln.de/")

  val repository = SesameRepository(ns)


  val bindings = Bindings(ns)


  def roleService = new RoleService(repository)

  "A role service" should {

    "check refroles properly" in {
      val result1 = roleService.checkWith(unbox(module1UserRole1))(Set(module1UserRole2))
      val result2 = roleService.checkWith(unbox(module1UserRole1))(Set(module1UserRole1, module2UserRole2))
      val result3 = roleService.checkWith(unbox(noneModule1Role1))(Set(module1UserRole1, noneModule1Role1, module2UserRole2))
      val result4 = roleService.checkWith(unbox(module1UserRole1))(Set(adminRole))


      result1 shouldBe false
      result2 shouldBe true
      result3 shouldBe true
      result4 shouldBe true
    }


    "retrieve authorities properly" in {
      import bindings.StudentBinding._
      import bindings.AuthorityBinding._

      val student1 = Student("mi1018", "last name", "first name", "email", "registrationId", Student.randomUUID)
      val student2 = Student("ai1223", "last name", "first name", "email", "registrationId", Student.randomUUID)
      val student3 = Student("ti1233", "last name", "first name", "email", "registrationId", Student.randomUUID)

      val authority1 = Authority(student1.id, Set(module1UserRole1, module2UserRole2))
      val authority2 = Authority(student2.id, Set(module2UserRole2, noneModule1Role1))

      repository.add(student1)
      repository.add(student2)
      repository.add(student3)
      repository.add(authority1)
      repository.add(authority2)

      val result1 = roleService.authorityFor(student1.id.toString)
      val result2 = roleService.authorityFor(student2.id.toString)
      val result3 = roleService.authorityFor(student3.id.toString)

      (result1, result2, result3) match {
        case (Some(r1), Some(r2), None) =>
          r1 shouldBe authority1
          r2 shouldBe authority2
        case _ => fail("No")
      }
    }
  }

}
