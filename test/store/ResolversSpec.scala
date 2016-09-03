package store

import base.TestBaseDefinition
import models.Degree
import models.security.{Permissions, Role}
import models.users.{Employee, Student, User}
import org.scalatest.WordSpec
import org.w3.banana.sesame.{Sesame, SesameModule}
import services.RoleService
import store.Prefixes.LWMPrefix
import store.bind.Bindings

import scala.util.{Failure, Success}

class ResolversSpec extends WordSpec with TestBaseDefinition with SesameModule {

  implicit val ns = Namespace("http://lwm.gm.fh-koeln.de/")
  val repo = SesameRepository(ns)

  val bindings = Bindings[this.repo.Rdf](ns)
  val lwm = LWMPrefix[this.repo.Rdf]

  val resolver = new LwmResolvers(repo)
  val roleService = new RoleService(repo)

  "A UsernameResolverSpec " should {

    "resolve a given username properly" in {
      import bindings.StudentDescriptor

      val student1 = Student("mi1018", "last name", "first name", "email", "registrationId", Degree.randomUUID)
      val student2 = Student("ai1223", "last name", "first name", "email", "registrationId", Degree.randomUUID)
      val student3 = Student("ti1233", "last name", "first name", "email", "registrationId", Degree.randomUUID)

      val previousSize = repo.size.get

      repo.add(student1)
      repo.add(student2)
      repo.add(student3)

      val result = resolver.userId(student1.systemId)

      result match {
        case Success(Some(uuid)) =>
          previousSize shouldEqual 0
          repo.size.get > previousSize shouldBe true
          uuid shouldEqual student1.id
        case Success(None) =>
          fail("uuid is none")
        case Failure(_) => fail("failed while retrieving data")
      }
    }

    "return `None` when username is not found" in {
      import bindings.StudentDescriptor

      val student1 = Student("mi1111", "last name", "first name", "email", "registrationId", Degree.randomUUID)
      val student2 = Student("ai1223", "last name", "first name", "email", "registrationId", Degree.randomUUID)
      val student3 = Student("ti1233", "last name", "first name", "email", "registrationId", Degree.randomUUID)

      repo.add(student1)
      repo.add(student2)
      repo.add(student3)

      val result = resolver.userId("ai111")

      result shouldBe Success(None)
    }

    // TODO repair with missingUserData

    /*
    "resolve a student, employee and their authorities when non-existent" in {
      import bindings.{
      RoleDescriptor,
      RefRoleDescriptor,
      StudentDescriptor,
      EmployeeDescriptor
      }
      val student1 = Student("mi1111", "last name", "first name", "email", "registrationId", Degree.randomUUID)
      val employee = Employee("system id", "last name", "first name", "email", "status")

      val studentRole = Role("Student", Set(Permissions.labworkApplication.create))
      val employeeRole = Role("Mitarbeiter", Set(Permissions.course.create, Permissions.timetable.create))

      val refrole1 = RefRole(None, studentRole.id)
      val refrole2 = RefRole(None, employeeRole.id)

      repo.add[Role](studentRole)
      repo.add[Role](employeeRole)

      repo.add[RefRole](refrole1)
      repo.add[RefRole](refrole2)

      resolver.missingUserData(student1)
      resolver.missingUserData(employee)

      val studentResult = repo.get[Student](User.generateUri(student1)(repo.namespace))
      val studentAuth = roleService.authorityFor(student1.id.toString)
      val studentRefRoles = studentAuth flatMap (auth => repo.getMany[RefRole](auth.get.refRoles map RefRole.generateUri))

      val employeeResult = repo.get[Employee](User.generateUri(employee)(repo.namespace))
      val employeeAuth = roleService.authorityFor(employee.id.toString)
      val employeeRefRoles = employeeAuth flatMap (auth => repo.getMany[RefRole](auth.get.refRoles map RefRole.generateUri))

      (studentResult, studentAuth, studentRefRoles) match {
        case (Success(Some(student)), Success(Some(auth)), Success(refRoles)) =>
          student shouldBe student1
          auth.user shouldBe student1.id
          refRoles.exists(_.role == studentRole.id) shouldBe true
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
          refRoles.exists(_.role == employeeRole.id) shouldBe true
          auth.refRoles.size shouldBe 1
        case (Failure(_), _, _) => fail("Could not retrieve user")
        case (_, Failure(_), _) => fail("Authority either not created or not found")
        case (_, _, Failure(_)) => fail("RefRoles either not created or not found")
        case _ => fail("failed when retrieving data")
      }
    }

    "stop trying to resolve somebody when other dependencies fail" in {
      import bindings.StudentDescriptor

      val student1 = Student("mi1111", "last name", "first name", "email", "registrationId", Degree.randomUUID)

      resolver.missingUserData(student1)

      val result = repo.get[Student](User.generateUri(student1)(repo.namespace))
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
      import bindings.{
      RoleDescriptor,
      RefRoleDescriptor,
      EmployeeDescriptor
      }

      import ops._
      val student1 = Student("mi1111", "last name", "first name", "email", "registrationId", Degree.randomUUID)
      val employee = Employee("system id", "last name", "first name", "email", "status")

      val employeeRole = Role("Mitarbeiter", Set(Permissions.course.create, Permissions.timetable.create))
      val refrole1 = RefRole(None, employeeRole.id)

      repo.add[Role](employeeRole)
      repo.add[RefRole](refrole1)

      resolver.missingUserData(student1) match {
        case Success(_) => fail("Should've not found an appropriate `RefRole` or Role")
        case Failure(e) =>
        e.getMessage shouldBe "No appropriate RefRole or Role found while resolving user"
      }

      resolver.missingUserData(employee) match {
        case Success(g) =>
          val demployee = g.as[Employee](EmployeeDescriptor.binder)
          demployee shouldBe Success(employee)
        case Failure(_) => fail("Should've found an appropriate `RefRole` or Role")
      }
    }
    */

    "successfully resolve a degrees based on theirs abbreviations" in {
      import bindings.DegreeDescriptor

      import scala.util.Random.nextInt

      val degrees = (0 until 10).map(i => Degree(i.toString, i.toString)).toList
      repo.addMany[Degree](degrees)

      val result = (0 until 8).map(_ => resolver.degree(degrees(nextInt(degrees.size)).abbreviation)).toList

      result.foreach {
        case Success(id) => degrees.exists(_.id == id) shouldBe true
        case Failure(e) => fail("No degree found for given abbreviation", e)
      }
    }

    "throw an exception when degree cant be resolved cause its not found" in {
      import bindings.DegreeDescriptor

      val degree1 = Degree("label", "abbrev")
      val degree2 = Degree("label2", "abbrev")
      val abbreviation = "not existent"

      repo.addMany[Degree](List(degree1, degree2))

      val result = resolver.degree(abbreviation)

      result match {
        case Success(_) => fail("Found degree, but there shouldn't be some")
        case Failure(e) => e.getMessage shouldBe s"No viable degree found for abbreviation $abbreviation"
      }
    }
  }

  override protected def beforeEach(): Unit = {
    super.beforeEach()
    repo.reset()
  }
}
