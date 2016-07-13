package services

import java.util.UUID

import base.SesameDbSpec
import models.labwork.Labwork
import models.security._
import models.semester.Semester
import models.users.Student
import models.Degree
import scala.util.{Failure, Success}

class RoleServiceSpec extends SesameDbSpec {

  val sufficientPermissions = Set(Permission("view"), Permission("create"), Permission("delete"))
  val insufficientPermissions = Set(Permission("view"), Permission("delete"))

  val module1 = UUID.randomUUID()
  val module2 = UUID.randomUUID()

  val lab1 = Labwork("lab1", "", Semester.randomUUID, module1, Degree.randomUUID)
  val lab2 = Labwork("lab2", "", Semester.randomUUID, module2, Degree.randomUUID)

  val role1 = Role("testRole1", sufficientPermissions)
  val role2 = Role("testRole2", insufficientPermissions)
  val role3 = Role("Admin", Set(Permissions.prime))

  val roles = Vector(role1, role2, role3)

  val noneModule1Role1 = RefRole(None, role1.id)
  val noneModule1Role2 = RefRole(None, role2.id)

  val module1UserRole1 = RefRole(Some(module1), role1.id)
  val module1UserRole2 = RefRole(Some(module1), role2.id)
  val module2UserRole2 = RefRole(Some(module2), role2.id)
  val adminRefRole = RefRole(None, role3.id)

  def unbox(r: RefRole): (Option[UUID], Set[Permission]) = (r.course, roles.find(_.id == r.role).get.permissions)

  def authority(refRoles: Set[RefRole]): Authority = Authority(UUID.randomUUID(), refRoles map (_.id))

  def roleService = new RoleService(repo)

  "A role service" should {

    "check refroles properly" in {
      import bindings.{
      RoleDescriptor,
      LabworkDescriptor,
      RefRoleDescriptor
      }
      import util.Random.nextInt

      repo.addMany(roles)
      repo.add(lab1)
      repo.add(lab2)
      repo.add(module1UserRole1)
      repo.add(module1UserRole2)
      repo.add(module2UserRole2)
      repo.add(noneModule1Role1)
      repo.add(noneModule1Role2)
      repo.add(adminRefRole)

      val perm1 = role1.permissions.toVector
      val perm2 = role2.permissions.toVector

      val result1 = roleService.checkWith((Some(module1), perm1(nextInt(perm1.size))))(authority(Set(module1UserRole2)))
      val result2 = roleService.checkWith((Some(module1), perm1(nextInt(perm1.size))))(authority(Set(module1UserRole1, module2UserRole2)))
      val result3 = roleService.checkWith((None, perm1(nextInt(perm1.size))))(authority(Set(module1UserRole1, noneModule1Role1, module2UserRole2)))
      val result4 = roleService.checkWith((Some(module1), perm1(nextInt(perm1.size))))(authority(Set(adminRefRole)))
      val result5 = roleService.checkWith((Some(module2), perm2(nextInt(perm2.size))))(authority(Set(module1UserRole1)))
      val result6 = roleService.checkWith((Some(UUID.randomUUID()), perm1(nextInt(perm1.size))))(authority(Set(adminRefRole)))

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
      import bindings.{
      RoleDescriptor,
      AuthorityDescriptor,
      StudentDescriptor,
      RefRoleDescriptor
      }

      val student1 = Student("mi1018", "last name", "first name", "email", "registrationId", Degree.randomUUID)
      val student2 = Student("ai1223", "last name", "first name", "email", "registrationId", Degree.randomUUID)
      val student3 = Student("ti1233", "last name", "first name", "email", "registrationId", Degree.randomUUID)

      val authority1 = Authority(student1.id, Set(module1UserRole1.id, module2UserRole2.id))
      val authority2 = Authority(student2.id, Set(module2UserRole2.id, noneModule1Role1.id))

      repo.add(student1)
      repo.add(student2)
      repo.add(student3)
      repo.add(module1UserRole1)
      repo.add(module2UserRole2)
      repo.add(role1)
      repo.add(role2)
      repo.add(role3)
      repo.add(authority1)
      repo.add(authority2)

      val result1 = roleService.authorityFor(student1.id.toString)
      val result2 = roleService.authorityFor(student2.id.toString)
      val result3 = roleService.authorityFor(student3.id.toString)

      (result1, result2, result3) match {
        case (Success(Some(r1)), Success(Some(r2)), Success(None)) =>
          r1 shouldBe authority1
          r2 shouldBe authority2
        case _ => fail("Should have found two of the authorities")
      }
    }

    "retrieve roles by roles label" in {
      import bindings.RoleDescriptor

      repo.addMany(roles)

      roleService.rolesByLabel("testRole1", "Admin") match {
        case Success(z) =>
          z contains role1 shouldBe true
          z contains role3 shouldBe true
          z.size shouldBe 2
        case Failure(e) => fail(s"Could not retrieve role: ${e.getMessage}")
      }
    }

    "retrieve refRoles by role label" in {
      import bindings.{RoleDescriptor, RefRoleDescriptor}

      repo.addMany(roles)
      repo.addMany(List(module1UserRole1, module1UserRole2, noneModule1Role1, adminRefRole))

      roleService.refRolesByLabel("testRole1", "Admin") match {
        case Success(z) =>
          z contains noneModule1Role1 shouldBe true
          z contains module1UserRole1 shouldBe true
          z contains adminRefRole shouldBe true
          z.size shouldBe 3
        case Failure(e) => fail(s"Could not retrieve refRole: ${e.getMessage}")
      }
    }
  }

  override protected def beforeEach(): Unit = repo.connect(_.clear())
}
