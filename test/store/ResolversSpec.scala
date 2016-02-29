package store

import base.TestBaseDefinition
import models.Degree
import models.users.{Employee, Student}
import org.scalatest.WordSpec
import org.w3.banana.sesame.{Sesame, SesameModule}
import store.Prefixes.LWMPrefix
import store.bind.Bindings
import services.RoleService
import models.security.{RefRole, Roles}
import utils.Ops._
import utils.Ops.MonadInstances._
import utils.Ops.TraverseInstances._

import scala.util.{Failure, Success}

class ResolversSpec extends WordSpec with TestBaseDefinition with SesameModule {

  implicit val ns = Namespace("http://lwm.gm.fh-koeln.de/")

  val bindings = Bindings[Sesame](ns)
  val lwm = LWMPrefix[Sesame]

  import bindings.StudentBinding._
  import bindings.AuthorityBinding._
  import bindings.permissionBinder
  import bindings.EmployeeBinding._
  import bindings.RefRoleBinding._

  val repo = SesameRepository(ns)

  val resolver = new LwmResolvers(repo)
  val roleService = new RoleService(repo)

  "A UsernameResolverSpec " should {
    "resolve a given username properly" in {
      val student1 = Student("mi1018", "last name", "first name", "email", "registrationId", Degree.randomUUID, Student.randomUUID)
      val student2 = Student("ai1223", "last name", "first name", "email", "registrationId", Degree.randomUUID, Student.randomUUID)
      val student3 = Student("ti1233", "last name", "first name", "email", "registrationId", Degree.randomUUID, Student.randomUUID)

      val previousSize = repo.size

      repo.add(student1)
      repo.add(student2)
      repo.add(student3)

      val result = resolver.username(student1.systemId)

      result match {
        case Success(Some(uuid)) =>
          previousSize shouldEqual 0
          repo.size > previousSize shouldBe true
          uuid shouldEqual student1.id
        case Success(None) =>
          fail("uuid is none")
        case Failure(_) => fail("failed while retrieving data")
      }
    }

    "return `None` when username is not found" in {
      val student1 = Student("mi1111", "last name", "first name", "email", "registrationId", Degree.randomUUID, Student.randomUUID)
      val student2 = Student("ai1223", "last name", "first name", "email", "registrationId", Degree.randomUUID, Student.randomUUID)
      val student3 = Student("ti1233", "last name", "first name", "email", "registrationId", Degree.randomUUID, Student.randomUUID)

      repo.add(student1)
      repo.add(student2)
      repo.add(student3)

      val result = resolver.username("ai111")

      result shouldBe Success(None)
    }

    "resolve a student, employee and their authorities when non-existent" in {
      val student1 = Student("mi1111", "last name", "first name", "email", "registrationId", Degree.randomUUID, Student.randomUUID)
      val employee = Employee("system id", "last name", "first name", "email", Employee.randomUUID)

      val refrole1 = RefRole(None, Roles.user.id)
      val refrole2 = RefRole(None, Roles.student.id)

      repo.add[RefRole](refrole1)
      repo.add[RefRole](refrole2)

      resolver.missingUserData(student1)
      resolver.missingUserData(employee)

      val studentResult = repo.get[Student](Student.generateUri(student1)(repo.namespace))
      val studentAuth = roleService.authorityFor(student1.id.toString)
      val studentRefRoles = studentAuth flatMap (auth => repo.getMany[RefRole](auth.get.refRoles map RefRole.generateUri))

      val employeeResult = repo.get[Employee](Employee.generateUri(employee)(repo.namespace))
      val employeeAuth = roleService.authorityFor(employee.id.toString)
      val employeeRefRoles = employeeAuth flatMap (auth => repo.getMany[RefRole](auth.get.refRoles map RefRole.generateUri))

      (studentResult, studentAuth, studentRefRoles) match {
        case (Success(Some(student)), Success(Some(auth)), Success(refRoles)) =>
          student shouldBe student1
          auth.user shouldBe student1.id
          refRoles.exists(_.role == Roles.student.id) shouldBe true
          auth.refRoles.size shouldBe 1
        case (Failure(_), _, _) => fail("Could not retrieve student")
        case (_, Failure(_), _) => fail("Authority either not created or not found")
        case (_, _, Failure(_)) => fail("RefRoles either not created or not found")
        case _ => fail("failed when retrieving data")
      }

      (employeeResult, employeeAuth, employeeRefRoles) match {
        case (Success(Some(emp)), Success(Some(auth)), Success(refRoles)) =>
          emp shouldBe employee
          auth.user shouldBe employee.id
          refRoles.exists(_.role == Roles.user.id) shouldBe true
          auth.refRoles.size shouldBe 1
        case (Failure(_), _, _) => fail("Could not retrieve user")
        case (_, Failure(_), _) => fail("Authority either not created or not found")
        case (_, _, Failure(_)) => fail("RefRoles either not created or not found")
        case _ => fail("failed when retrieving data")
      }
    }

    "stop trying to resolve somebody when other dependencies fail" in {
      val student1 = Student("mi1111", "last name", "first name", "email", "registrationId", Degree.randomUUID, Student.randomUUID)

      resolver.missingUserData(student1)

      val result = repo.get[Student](Student.generateUri(student1)(repo.namespace))
      val possibleAuthority = roleService.authorityFor(student1.id.toString)


      (result, possibleAuthority) match {
        case (Success(Some(student)), Success(Some(auth))) => fail("Neither the student n'or his authority should be found")
        case (s, Success(Some(_))) => fail("No authority should have been created")
        case (s, Success(None)) =>
          s shouldBe Success(None)
          repo.size shouldBe 0
        case _ => fail("failed while retrieving data")
      }
    }

    "stop and alert back when no appropriate `RefRole` was found" in {
      import bindings.EmployeeBinding._
      import ops._
      val student1 = Student("mi1111", "last name", "first name", "email", "registrationId", Degree.randomUUID, Student.randomUUID)
      val employee = Employee("system id", "last name", "first name", "email", Employee.randomUUID)

      val refrole1 = RefRole(None, Roles.user.id)

      repo.add[RefRole](refrole1)

      resolver.missingUserData(student1) match {
        case Success(_) => fail("Should've not found an appropriate `RefRole`")
        case Failure(e) =>
        e.getMessage shouldBe "No appropriate RefRole found while resolving user"
      }

      resolver.missingUserData(employee) match {
        case Success(g) =>
          val demployee = g.as[Employee]
          demployee shouldBe Success(employee)
        case Failure(_) => fail("Should've found an appropriate `RefRole`")
      }

    }
  }

  override protected def beforeEach(): Unit = {
    super.beforeEach()
    repo.reset()
  }
}
