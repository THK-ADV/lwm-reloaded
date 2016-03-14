package controllers

import java.util.UUID

import base.TestBaseDefinition
import models.Degree
import models.users.{Employee, Student, StudentAtom, User}
import org.mockito.Matchers._
import org.scalatest.WordSpec
import org.mockito.Mockito._
import org.scalatest.mock.MockitoSugar.mock
import play.api.test.FakeRequest
import play.api.http.HttpVerbs
import play.api.libs.json.{JsArray, JsValue, Json}
import play.api.test.Helpers._
import services.RoleService
import store.bind.Bindings
import store.{Namespace, SesameRepository}
import Student._

import scala.util.Success

class UserControllerSpec extends WordSpec with TestBaseDefinition {
  
  val ns = Namespace("http://lwm.gm.th-koeln.de")
  val repo = mock[SesameRepository]
  val roleService = mock[RoleService]

  val bindings = Bindings[repo.Rdf](ns)

  val controller: UserController = new UserController(roleService, repo, ns) {
    //to be specialized
    override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }
  }

  "A UserController" should {

    "get one specific user, regardless of his subcategory" in {
      val student1 = Student("ai1818", "Hans", "Wurst", "bla@mail.de", "11223344", UUID.randomUUID())

      when(repo.get[User](anyObject())(anyObject())).thenReturn(Success(Some(student1)))

      val request = FakeRequest(
        HttpVerbs.GET,
        "/users/" + student1.id
      )

      val result = controller.get(student1.id.toString)(request)

      status(result) shouldBe OK
      contentAsJson(result) shouldBe Json.toJson(student1)
    }

    "get all users, regardless of their subcategory" in {
      val student1 = Student("ai1818", "Hans", "Wurst", "bla@mail.de", "11223344", UUID.randomUUID())
      val student2 = Student("mi1818", "Sanh", "Tsruw", "alb@mail.de", "44332211", UUID.randomUUID())
      val student3 = Student("wi1818", "Nahs", "Rustw", "lab@mail.de", "22331144", UUID.randomUUID())
      val employee1 = Employee("mlark", "Lars", "Marklar", "mark@mail.de", "status")
      val employee2 = Employee("mlark", "Sarl", "Ralkram", "kram@mail.de", "status")
      val employee3 = Employee("rlak", "Rasl", "Kramral", "ramk@mail.de", "status")
      val users: Set[User] = Set(student1, student2, student3, employee1, employee2, employee3)

      when(repo.get[User](anyObject(), anyObject())).thenReturn(Success(users))

      val request = FakeRequest(
        HttpVerbs.GET,
        "/users"
      )

      val result = controller.all()(request)

      val jsonVals = users map UserController.toJson

      status(result) shouldBe OK
      contentAsJson(result).asInstanceOf[JsArray].value foreach { entry =>
          jsonVals contains entry shouldBe true
      }
    }

    "atomize one specific user, regardless of his subcategory" in {
      val degree = Degree("Degree", "DD", Degree.randomUUID)
      val student1 = Student("ai1818", "Hans", "Wurst", "bla@mail.de", "11223344", degree.id)
      val student2 = Student("mi1818", "Sanh", "Tsruw", "alb@mail.de", "44332211", UUID.randomUUID())
      val student3 = Student("wi1818", "Nahs", "Rustw", "lab@mail.de", "22331144", UUID.randomUUID())
      val employee1 = Employee("mlark", "Lars", "Marklar", "mark@mail.de", "status")
      val employee2 = Employee("mlark", "Sarl", "Ralkram", "kram@mail.de", "status")
      val employee3 = Employee("rlak", "Rasl", "Kramral", "ramk@mail.de", "status")

      val atom = StudentAtom(student1.systemId, student1.lastname, student1.firstname, student1.email, student1.registrationId, degree, student1.id)

      doReturn(Success(Some(student1))).doReturn(Success(Some(degree))).when(repo).get(anyObject())(anyObject())

      val request = FakeRequest(
        HttpVerbs.GET,
        "/atomic/users/" + student1.id
      )

      val result = controller.getAtomic(User.generateUri(student1)(ns))(request)

      status(result) shouldBe OK
      contentAsJson(result) shouldBe Json.toJson(atom)
    }

    "atomize all users, regardless of their subcategory" in {
      val degree1 = Degree("Degree1", "DD1", Degree.randomUUID)

      def jsonAtomic(s: Set[User]): Set[JsValue] = s map {
        case s: Student =>
          Json.toJson(StudentAtom(s.systemId, s.lastname, s.firstname, s.email, s.registrationId, degree1, s.id))
        case e: Employee =>
          Json.toJson(e)
      }
      val student1 = Student("ai1818", "Hans", "Wurst", "bla@mail.de", "11223344", degree1.id)
      val student2 = Student("ai2182", "Sanh", "Tsruw", "alb@mail.de", "44332211", degree1.id)
      val student3 = Student("ai3512", "Nahs", "Rustw", "lab@mail.de", "22331144", degree1.id)
      val employee1 = Employee("mlark", "Lars", "Marklar", "mark@mail.de", "status")
      val employee2 = Employee("mlark", "Sarl", "Ralkram", "kram@mail.de", "status")
      val employee3 = Employee("rlak", "Rasl", "Kramral", "ramk@mail.de", "status")
      val users: Set[User] = Set(student1, student2, student3, employee1, employee2, employee3)

        when(repo.get[User](anyObject(), anyObject())).thenReturn(Success(users))
        when(repo.get[Degree](anyObject())(anyObject())).thenReturn(Success(Some(degree1)))

      val request = FakeRequest(
        HttpVerbs.GET,
        "/atomic/users"
      )

      val result = controller.allAtomic()(request)

      val jsonVals = jsonAtomic(users)

      status(result) shouldBe OK
      contentAsJson(result).asInstanceOf[JsArray].value foreach { user =>
        jsonVals contains user shouldBe true
      }
    }

    "get a single student" in {
      val student1 = Student("ai1818", "Hans", "Wurst", "bla@mail.de", "11223344", UUID.randomUUID())

      when(repo.get[Student](anyObject())(anyObject())).thenReturn(Success(Some(student1)))

      val request = FakeRequest(
        HttpVerbs.GET,
        "/students/" + student1.id
      )

      val result = controller.student(student1.id.toString)(request)

      status(result) shouldBe OK
      contentAsJson(result) shouldBe Json.toJson(student1)

    }

    "atomize a single student" in {
      val degree1 = Degree("Degree1", "DD1", Degree.randomUUID)
      val student1 = Student("ai1818", "Hans", "Wurst", "bla@mail.de", "11223344", degree1.id)
      val atom = StudentAtom(student1.systemId, student1.lastname, student1.firstname, student1.email, student1.registrationId, degree1, student1.id)

      doReturn(Success(Some(student1))).doReturn(Success(Some(degree1))).when(repo).get(anyObject())(anyObject())

      val request = FakeRequest(
        HttpVerbs.GET,
        "/atomic/students/" + student1.id
      )

      val result = controller.studentAtomic(User.generateUri(student1)(ns))(request)

      status(result) shouldBe OK
      contentAsJson(result) shouldBe Json.toJson(atom)
    }

    "get all students" in {
      val student1 = Student("ai1818", "Hans", "Wurst", "bla@mail.de", "11223344", UUID.randomUUID())
      val student2 = Student("mi1818", "Sanh", "Tsruw", "alb@mail.de", "44332211", UUID.randomUUID())
      val student3 = Student("wi1818", "Nahs", "Rustw", "lab@mail.de", "22331144", UUID.randomUUID())

      val students: Set[Student] = Set(student1, student2, student3)

      when(repo.get[Student](anyObject(), anyObject())).thenReturn(Success(students))

      val request = FakeRequest(
        HttpVerbs.GET,
        "/students"
      )

      val result = controller.allStudents()(request)

      val jsonVals = students map (a => Json.toJson(a))

      status(result) shouldBe OK
      contentAsJson(result).asInstanceOf[JsArray].value foreach { entry =>
        jsonVals contains entry shouldBe true
      }
    }

    "atomize all students" in {
      val degree1 = Degree("Degree1", "DD1", Degree.randomUUID)

      def jsonAtomic(students: Set[Student]): Set[JsValue] = students map { s =>
          Json.toJson(StudentAtom(s.systemId, s.lastname, s.firstname, s.email, s.registrationId, degree1, s.id))
      }

      val student1 = Student("ai1818", "Hans", "Wurst", "bla@mail.de", "11223344", degree1.id)
      val student2 = Student("ai2182", "Sanh", "Tsruw", "alb@mail.de", "44332211", degree1.id)
      val student3 = Student("ai3512", "Nahs", "Rustw", "lab@mail.de", "22331144", degree1.id)
      val students: Set[Student] = Set(student1, student2, student3)

      when(repo.get[Student](anyObject(), anyObject())).thenReturn(Success(students))
      when(repo.get[Degree](anyObject())(anyObject())).thenReturn(Success(Some(degree1)))

      val request = FakeRequest(
        GET,
        "/atomic/students"
      )

      val result = controller.allAtomicStudents()(request)

      val jsonVals = jsonAtomic(students)

      status(result) shouldBe OK
      contentAsJson(result).asInstanceOf[JsArray].value foreach { entry =>
        jsonVals contains entry shouldBe true
      }
    }

    "get a single employee" in {
      val employee1 = Employee("mlark", "Lars", "Marklar", "mark@mail.de", "status")

        when(repo.get[Employee](anyObject())(anyObject())).thenReturn(Success(Some(employee1)))

        val request = FakeRequest(
          HttpVerbs.GET,
          "/employees/" + employee1.id
        )

        val result = controller.employee(employee1.id.toString)(request)

        status(result) shouldBe OK
        contentAsJson(result) shouldBe Json.toJson(employee1)
    }

    "get all employees" in {
      val employee1 = Employee("mlark", "Lars", "Marklar", "mark@mail.de", "status")
      val employee2 = Employee("mlark", "Sarl", "Ralkram", "kram@mail.de", "status")
      val employee3 = Employee("rlak", "Rasl", "Kramral", "ramk@mail.de", "status")
      val employees: Set[Employee] = Set(employee1, employee2, employee3)

      when(repo.get[Employee](anyObject(), anyObject())).thenReturn(Success(employees))

      val request = FakeRequest(
        HttpVerbs.GET,
        "/employees"
      )

      val result = controller.all()(request)

      val jsonVals = employees map (a => Json.toJson(a))

      status(result) shouldBe OK
      contentAsJson(result).asInstanceOf[JsArray].value foreach { entry =>
        jsonVals contains entry shouldBe true
      }
    }

    "get students specific to particular degree" in {
      val degree1 = Degree("Degree1", "DD1", Degree.randomUUID)

      val student1 = Student("ai1818", "Hans", "Wurst", "bla@mail.de", "11223344", degree1.id)
      val student2 = Student("ai2182", "Sanh", "Tsruw", "alb@mail.de", "44332211", degree1.id)
      val student3 = Student("mi3512", "Nahs", "Rustw", "lab@mail.de", "22331144", Degree.randomUUID)
      val students: Set[Student] = Set(student1, student2, student3)

      when(repo.get[Student](anyObject(), anyObject())).thenReturn(Success(students))

      val request = FakeRequest(
        GET,
        "/students?degree=" + degree1.id
      )

      val sjson = Vector(Json.toJson(student1), Json.toJson(student2))
      val result = controller.all()(request)

      status(result) shouldBe OK
      contentAsJson(result).asInstanceOf[JsArray].value foreach { student =>
        sjson contains student shouldBe true
      }
    }

    "get atomized students specific to a particular degree" in {
      val degree1 = Degree("Degree1", "DD1", Degree.randomUUID)

      def jsonAtomic(students: TraversableOnce[Student]): Set[JsValue] = (students map { s =>
        Json.toJson(StudentAtom(s.systemId, s.lastname, s.firstname, s.email, s.registrationId, degree1, s.id))
      }).toSet

      val student1 = Student("ai1818", "Hans", "Wurst", "bla@mail.de", "11223344", degree1.id)
      val student2 = Student("ai2182", "Sanh", "Tsruw", "alb@mail.de", "44332211", degree1.id)
      val student3 = Student("mi3512", "Nahs", "Rustw", "lab@mail.de", "22331144", UUID.randomUUID())
      val students: Set[Student] = Set(student1, student2, student3)

      when(repo.get[Student](anyObject(), anyObject())).thenReturn(Success(students))
      when(repo.get[Degree](anyObject())(anyObject())).thenReturn(Success(Some(degree1)))

      val request = FakeRequest(
        GET,
        "/atomic/students?degree=" + degree1.id
      )

      val result = controller.allAtomicStudents()(request)

      val jsonVals = jsonAtomic(Set(student1, student2))

      status(result) shouldBe OK
      contentAsJson(result).asInstanceOf[JsArray].value foreach { entry =>
        jsonVals contains entry shouldBe true
      }
    }

    "get employees specific to a particular status" in {
      val employee1 = Employee("mlark", "Lars", "Marklar", "mark@mail.de", "lecturer")
      val employee2 = Employee("mlark", "Sarl", "Ralkram", "kram@mail.de", "employee")
      val employee3 = Employee("rlak", "Rasl", "Kramral", "ramk@mail.de", "lecturer")
      val employees: Set[Employee] = Set(employee1, employee2, employee3)
      val lecturers: Set[Employee] = Set(employee1, employee3)

      when(repo.get[Employee](anyObject(), anyObject())).thenReturn(Success(employees))

      val request = FakeRequest(
        HttpVerbs.GET,
        "/employees?status=lecturer"
      )

      val result = controller.all()(request)

      val jsonVals = lecturers map (a => Json.toJson(a))

      status(result) shouldBe OK
      contentAsJson(result).asInstanceOf[JsArray].value foreach { entry =>
        jsonVals contains entry shouldBe true
      }
    }

    "get users specific to some particular filter attribute" in {
      val degree = UUID.randomUUID()
      val student1 = Student("ai1818", "Hans", "Wurst", "bla@mail.de", "11223344", degree)
      val student2 = Student("ai2182", "Sanh", "Tsruw", "alb@mail.de", "44332211", UUID.randomUUID())
      val student3 = Student("mi3512", "Nahs", "Rustw", "lab@mail.de", "22331144", degree)
      val employee1 = Employee("mlark", "Lars", "Marklar", "mark@mail.de", "lecturer")
      val employee2 = Employee("mlark", "Sarl", "Ralkram", "kram@mail.de", "employee")
      val employee3 = Employee("rlak", "Rasl", "Kramral", "ramk@mail.de", "lecturer")
      val users: Set[User] = Set(student1, student2, student3, employee1, employee2, employee3)
      val lecturers: Set[Employee] = Set(employee1, employee3)
      val degreers: Set[Student] = Set(student1, student3)

      when(repo.get[User](anyObject(), anyObject())).thenReturn(Success(users))

      val request1 = FakeRequest(
        HttpVerbs.GET,
        "/users?status=lecturer"
      )

      val request2 = FakeRequest(
        HttpVerbs.GET,
        "/users?degree=" + degree
      )

      val result1 = controller.all()(request1)
      val result2 = controller.all()(request2)

      val jsonVals1 = lecturers map (a => Json.toJson(a))
      val jsonVals2 = degreers map (a => Json.toJson(a))

      status(result1) shouldBe OK
      contentAsJson(result1).asInstanceOf[JsArray].value foreach { entry =>
        jsonVals1 contains entry shouldBe true
      }

      status(result2) shouldBe OK
      contentAsJson(result2).asInstanceOf[JsArray].value foreach { entry =>
        jsonVals2 contains entry shouldBe true
      }
    }

    "get atomized users specific to some particular filter attribute" in {
      val degree1 = Degree("Degree1", "DD1", Degree.randomUUID)

      def jsonAtomic(students: TraversableOnce[Student]): Set[JsValue] = (students map { s =>
        Json.toJson(StudentAtom(s.systemId, s.lastname, s.firstname, s.email, s.registrationId, degree1, s.id))
      }).toSet

      val student1 = Student("ai1818", "Hans", "Wurst", "bla@mail.de", "11223344", degree1.id)
      val student2 = Student("ai2182", "Sanh", "Tsruw", "alb@mail.de", "44332211", UUID.randomUUID())
      val student3 = Student("mi3512", "Nahs", "Rustw", "lab@mail.de", "22331144", degree1.id)
      val employee1 = Employee("mlark", "Lars", "Marklar", "mark@mail.de", "lecturer")
      val employee2 = Employee("mlark", "Sarl", "Ralkram", "kram@mail.de", "employee")
      val employee3 = Employee("rlak", "Rasl", "Kramral", "ramk@mail.de", "lecturer")
      val users: Set[User] = Set(student1, student2, student3, employee1, employee2, employee3)
      val lecturers: Set[Employee] = Set(employee1, employee3)
      val degreers: Set[Student] = Set(student1, student3)

      when(repo.get[User](anyObject(), anyObject())).thenReturn(Success(users))
      when(repo.get[Degree](anyObject())(anyObject())).thenReturn(Success(Some(degree1)))

      val request1 = FakeRequest(
        HttpVerbs.GET,
        "/atomic/users?degree=" + degree1.id
      )

      val request2 = FakeRequest(
        HttpVerbs.GET,
        "/atomic/users?status=lecturer"
      )

      val result1 = controller.allAtomic()(request1)
      val result2 = controller.allAtomic()(request2)

      val jsonVals1 = jsonAtomic(degreers)
      val jsonVals2 = lecturers map (a => Json.toJson(a))

      status(result1) shouldBe OK
      contentAsJson(result1).asInstanceOf[JsArray].value foreach { entry =>
        jsonVals1 contains entry shouldBe true
      }

      status(result2) shouldBe OK
      contentAsJson(result2).asInstanceOf[JsArray].value foreach { entry =>
        jsonVals2 contains entry shouldBe true
      }
    }
  }
}
