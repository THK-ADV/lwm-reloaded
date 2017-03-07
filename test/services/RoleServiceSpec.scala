package services

import java.util.UUID

import base.SesameDbSpec
import models._

import scala.util.{Failure, Success, Try}

class RoleServiceSpec extends SesameDbSpec {

  val sufficientPermissions = Set(SesamePermission("view"), SesamePermission("create"), SesamePermission("delete"))
  val insufficientPermissions = Set(SesamePermission("view"), SesamePermission("delete"))

  val module1 = UUID.randomUUID
  val module2 = UUID.randomUUID

  val role1 = SesameRole("testRole1", sufficientPermissions)
  val role2 = SesameRole("testRole2", insufficientPermissions)
  val role3 = SesameRole("Admin", Set(Permissions.prime))

  val roles = Vector(role1, role2, role3)

  val noneModule1Role1 = SesameAuthority(UUID.randomUUID, role1.id)
  val noneModule1Role2 = SesameAuthority(UUID.randomUUID, role2.id)

  val module1UserRole1 = SesameAuthority(UUID.randomUUID, role1.id, Some(module1))
  val module1UserRole2 = SesameAuthority(UUID.randomUUID, role2.id, Some(module1))
  val module2UserRole2 = SesameAuthority(UUID.randomUUID, role2.id, Some(module2))
  val adminRefRole = SesameAuthority(UUID.randomUUID, role3.id)

  val roleService = new RoleService(repo)

  "A role service" should {

    "check authorities properly" in {
      import bindings.RoleDescriptor
      import scala.util.Random.nextInt

      repo.addMany(roles)

      val perm1 = role1.permissions.toVector
      val perm2 = role2.permissions.toVector

      val result1 = roleService.checkAuthority((Some(module1), perm1(nextInt(perm1.size))))(module1UserRole2)
      val result2 = roleService.checkAuthority((Some(module1), perm1(nextInt(perm1.size))))(module1UserRole1, module2UserRole2)
      val result3 = roleService.checkAuthority((None, perm1(nextInt(perm1.size))))(module1UserRole1, noneModule1Role1, module2UserRole2)
      val result4 = roleService.checkAuthority((Some(module1), perm1(nextInt(perm1.size))))(adminRefRole)
      val result5 = roleService.checkAuthority((Some(module2), perm2(nextInt(perm2.size))))(module1UserRole1)
      val result6 = roleService.checkAuthority((Some(UUID.randomUUID()), perm1(nextInt(perm1.size))))(adminRefRole)

      for {
        r1 <- result1
        r2 <- result2
        r3 <- result3
        r4 <- result4
        r5 <- result5
        r6 <- result6
      } yield {
        r1 shouldBe false
        r2 shouldBe true
        r3 shouldBe true
        r4 shouldBe true
        r5 shouldBe false
        r6 shouldBe true
      }
    }

    "retrieve authorities properly" in {
      import bindings.{RoleDescriptor, AuthorityDescriptor, StudentDescriptor}

      val student1 = SesameStudent("mi1018", "last name", "first name", "email", "registrationId", PostgresDegree.randomUUID)
      val student2 = SesameStudent("ai1223", "last name", "first name", "email", "registrationId", PostgresDegree.randomUUID)
      val student3 = SesameStudent("ti1233", "last name", "first name", "email", "registrationId", PostgresDegree.randomUUID)

      val authority1 = SesameAuthority(student1.id, module1UserRole1.role, module1UserRole1.course)
      val authority2 = SesameAuthority(student1.id, module2UserRole2.role, module1UserRole1.course)
      val authority3 = SesameAuthority(student2.id, module2UserRole2.role, module2UserRole2.course)
      val authority4 = SesameAuthority(student2.id, noneModule1Role1.role, noneModule1Role1.course)

      repo.addMany(List(student1, student2, student3))
      repo.addMany(List(role1, role2, role3))
      repo.addMany(List(authority1, authority2, authority3, authority4))

      val result1 = roleService.authorities(student1.id)
      val result2 = roleService.authorities(student2.id)
      val result3 = roleService.authorities(student3.id)

      (result1, result2, result3) match {
        case (Success(r1), Success(r2), Success(r3)) =>
          r1 shouldBe Set(authority1, authority2)
          r2 shouldBe Set(authority3, authority4)
          r3 shouldBe empty
        case _ => fail("Should have found two of the authorities")
      }
    }

    "retrieve roles by roles label" in {
      import bindings.RoleDescriptor

      repo.addMany(roles)

      roleService.rolesByLabel("testRole1", "Admin") match {
        case Success(r) =>
          r contains role1 shouldBe true
          r contains role3 shouldBe true
          r.size shouldBe 2
        case Failure(e) => fail(s"Could not retrieve role: ${e.getMessage}")
      }
    }

    "filter roles for lecturer when he is not rights manager already" in {
      import bindings.{RoleDescriptor, AuthorityDescriptor}

      val cm = SesameRole(Roles.CourseManagerLabel, Set.empty)
      val emp = SesameRole(Roles.EmployeeLabel, Set.empty)
      val rm = SesameRole(Roles.RightsManagerLabel, Set.empty)

      val employee = SesameEmployee("systemId", "lastname", "firstname", "email", User.EmployeeType)
      val authorities = (0 until 10).map(i => SesameAuthority(UUID.randomUUID, UUID.randomUUID)).toSet ++ Set(
        SesameAuthority(employee.id, UUID.randomUUID, Some(UUID.randomUUID)),
        SesameAuthority(employee.id, emp.id)
      )

      repo addMany Set(cm, emp, rm)
      repo addMany authorities

      roleService.rolesForCourse(employee.id) match {
        case Success(set) =>
          set shouldBe Set(cm, rm)
        case Failure(e) => fail("There should be some roles for lecturer but was not", e)
      }
    }

    "filter roles for lecturer when he is rights manager already" in {
      import bindings.{RoleDescriptor, AuthorityDescriptor}

      val cm = SesameRole(Roles.CourseManagerLabel, Set.empty)
      val emp = SesameRole(Roles.EmployeeLabel, Set.empty)
      val rm = SesameRole(Roles.RightsManagerLabel, Set.empty)

      val employee = SesameEmployee("systemId", "lastname", "firstname", "email", User.EmployeeType)
      val authorities = (0 until 10).map(i => SesameAuthority(UUID.randomUUID, UUID.randomUUID)).toSet ++ Set(
        SesameAuthority(employee.id, rm.id, None),
        SesameAuthority(employee.id, emp.id)
      )

      repo addMany Set(cm, emp, rm)
      repo addMany authorities

      roleService.rolesForCourse(employee.id) match {
        case Success(set) =>
          set shouldBe Set(cm)
        case Failure(e) => fail("There should be some roles for lecturer but was not", e)
      }
    }
  }

  override protected def beforeEach(): Unit = repo.connect(conn => Try(conn.clear())).get
}
