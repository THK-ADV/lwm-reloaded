package controllers

import java.util.UUID

import base.TestBaseDefinition
import models.Degree
import models.users.{Employee, Student, StudentAtom, User}
import org.mockito.Matchers._
import org.openrdf.model.Value
import org.scalatest.WordSpec
import org.mockito.Mockito._
import org.scalatest.mock.MockitoSugar.mock
import org.w3.banana.sesame.SesameModule
import play.api.{Application, ApplicationLoader}
import play.api.ApplicationLoader.Context
import play.api.test.{FakeRequest, WithApplicationLoader}
import play.api.http.HttpVerbs
import play.api.libs.json.{JsValue, Json}
import play.api.test.Helpers._
import services.{RoleService, SessionHandlingService}
import store.bind.Bindings
import store.sparql.{QueryEngine, QueryExecutor, SelectClause}
import store.{Namespace, SesameRepository}
import Student._
import utils.DefaultLwmApplication
import UserController.writes
import scala.util.Success

class UserControllerSpec extends WordSpec with TestBaseDefinition with SesameModule {

  val repository = mock[SesameRepository]
  val roleService = mock[RoleService]
  val sessionService = mock[SessionHandlingService]
  val qe = mock[QueryExecutor[SelectClause]]
  val query = QueryEngine.empty(qe)

  val namespace = Namespace("http://lwm.gm.th-koeln.de")
  val bindings = Bindings[repository.Rdf](namespace)

  val controller: UserController = new UserController(roleService, sessionService, repository, namespace) {

    override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }
  }

  class FakeApp extends WithApplicationLoader(new ApplicationLoader {
    override def load(context: Context): Application = new DefaultLwmApplication(context) {
      override def userController: UserController = controller
    }.application
  })

  "A UserController" should {

    "get one specific user, regardless of his subcategory" in {
      val student1 = Student("ai1818", "Hans", "Wurst", "bla@mail.de", "11223344", UUID.randomUUID())

      when(repository.get[User](anyObject())(anyObject())).thenReturn(Success(Some(student1)))

      val request = FakeRequest(
        HttpVerbs.GET,
        "/users/" + student1.id
      )

      val result = controller.user(student1.id.toString)(request)

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

      when(repository.get[User](anyObject(), anyObject())).thenReturn(Success(users))

      val request = FakeRequest(
        HttpVerbs.GET,
        "/users"
      )

      val result = controller.allUsers()(request)
      val jsonVals = users map (u => Json.toJson(u))

      status(result) shouldBe OK
      val stringResult = contentAsString(result)

      jsonVals.forall { json =>
        stringResult contains json.toString
      } shouldBe true
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

      doReturn(Success(Some(student1))).doReturn(Success(Some(degree))).when(repository).get(anyObject())(anyObject())

      val request = FakeRequest(
        HttpVerbs.GET,
        "/atomic/users/" + student1.id
      )

      val result = controller.userAtomic(User.generateUri(student1)(namespace))(request)

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

      when(repository.get[User](anyObject(), anyObject())).thenReturn(Success(users))
      when(repository.get[Degree](anyObject())(anyObject())).thenReturn(Success(Some(degree1)))

      val request = FakeRequest(
        HttpVerbs.GET,
        "/atomic/users"
      )

      val result = controller.allUserAtomic()(request)
      val jsonVals = jsonAtomic(users)
      val stringResult = contentAsString(result)

      status(result) shouldBe OK
      jsonVals.forall { json =>
        stringResult contains json.toString
      } shouldBe true
    }

    "get a single student" in {
      val student1 = Student("ai1818", "Hans", "Wurst", "bla@mail.de", "11223344", UUID.randomUUID())

      when(repository.get[Student](anyObject())(anyObject())).thenReturn(Success(Some(student1)))

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

      doReturn(Success(Some(student1))).doReturn(Success(Some(degree1))).when(repository).get(anyObject())(anyObject())

      val request = FakeRequest(
        HttpVerbs.GET,
        "/atomic/students/" + student1.id
      )

      val result = controller.studentAtomic(User.generateUri(student1)(namespace))(request)

      status(result) shouldBe OK
      contentAsJson(result) shouldBe Json.toJson(atom)
    }

    "get all students" in {
      val student1 = Student("ai1818", "Hans", "Wurst", "bla@mail.de", "11223344", UUID.randomUUID())
      val student2 = Student("mi1818", "Sanh", "Tsruw", "alb@mail.de", "44332211", UUID.randomUUID())
      val student3 = Student("wi1818", "Nahs", "Rustw", "lab@mail.de", "22331144", UUID.randomUUID())

      val students: Set[Student] = Set(student1, student2, student3)

      when(repository.get[Student](anyObject(), anyObject())).thenReturn(Success(students))

      val request = FakeRequest(
        HttpVerbs.GET,
        "/students"
      )

      val result = controller.allStudents()(request)

      val jsonVals = students map (a => Json.toJson(a))
      val stringResult = contentAsString(result)

      status(result) shouldBe OK
      jsonVals.forall { json =>
        stringResult contains json.toString
      } shouldBe true
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

      when(repository.get[Student](anyObject(), anyObject())).thenReturn(Success(students))
      when(repository.get[Degree](anyObject())(anyObject())).thenReturn(Success(Some(degree1)))

      val request = FakeRequest(
        GET,
        "/atomic/students"
      )

      val result = controller.allAtomicStudents()(request)
      val jsonVals = jsonAtomic(students)
      val stringResult = contentAsString(result)

      status(result) shouldBe OK
      jsonVals.forall { json =>
        stringResult contains json.toString
      } shouldBe true
    }

    "get a single employee" in {
      val employee1 = Employee("mlark", "Lars", "Marklar", "mark@mail.de", "status")

      when(repository.get[Employee](anyObject())(anyObject())).thenReturn(Success(Some(employee1)))

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

      when(repository.get[Employee](anyObject(), anyObject())).thenReturn(Success(employees))

      val request = FakeRequest(
        HttpVerbs.GET,
        "/employees"
      )

      val result = controller.allUsers()(request)

      val jsonVals = employees map (a => Json.toJson(a))

      status(result) shouldBe OK
      val stringResult = contentAsString(result)

      jsonVals.forall { json =>
        stringResult contains json.toString
      } shouldBe true
    }

    "get students specific to particular degree" in {
      val degree1 = Degree("Degree1", "DD1", Degree.randomUUID)

      val student1 = Student("ai1818", "Hans", "Wurst", "bla@mail.de", "11223344", degree1.id)
      val student2 = Student("ai2182", "Sanh", "Tsruw", "alb@mail.de", "44332211", degree1.id)
      val student3 = Student("mi3512", "Nahs", "Rustw", "lab@mail.de", "22331144", Degree.randomUUID)
      val students: Set[Student] = Set(student1, student2, student3)

      when(repository.get[Student](anyObject(), anyObject())).thenReturn(Success(students))

      val request = FakeRequest(
        GET,
        "/students?degree=" + degree1.id
      )

      val sjson = Vector(Json.toJson(student1), Json.toJson(student2))
      val result = controller.allUsers()(request)

      status(result) shouldBe OK
      val stringResult = contentAsString(result)

      sjson.forall { json =>
        stringResult contains json.toString
      } shouldBe true
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

      when(repository.get[Student](anyObject(), anyObject())).thenReturn(Success(students))
      when(repository.get[Degree](anyObject())(anyObject())).thenReturn(Success(Some(degree1)))

      val request = FakeRequest(
        GET,
        "/atomic/students?degree=" + degree1.id
      )

      val result = controller.allAtomicStudents()(request)
      val jsonVals = jsonAtomic(Set(student1, student2))
      val stringResult = contentAsString(result)

      status(result) shouldBe OK
      jsonVals.forall { json =>
        stringResult contains json.toString
      } shouldBe true
    }

    "get employees specific to a particular status" in {
      val employee1 = Employee("mlark", "Lars", "Marklar", "mark@mail.de", "lecturer")
      val employee2 = Employee("mlark", "Sarl", "Ralkram", "kram@mail.de", "employee")
      val employee3 = Employee("rlak", "Rasl", "Kramral", "ramk@mail.de", "lecturer")
      val employees: Set[Employee] = Set(employee1, employee2, employee3)
      val lecturers: Set[Employee] = Set(employee1, employee3)

      when(repository.get[Employee](anyObject(), anyObject())).thenReturn(Success(employees))

      val request = FakeRequest(
        HttpVerbs.GET,
        "/employees?status=lecturer"
      )

      val result = controller.allUsers()(request)

      val jsonVals = lecturers map (l => Json.toJson(l))

      status(result) shouldBe OK
      val stringResult = contentAsString(result)

      jsonVals.forall { json =>
        stringResult contains json.toString
      } shouldBe true
    }

    "get users specific to some particular filter attribute" in {
      import UserController.{firstnameAttribute, lastnameAttribute, statusAttribute, degreeAttribute, systemIdAttribute}

      val degree = UUID.randomUUID()
      val student1 = Student("ai1818", "Hans", "Wurst", "bla@mail.de", "11223344", degree)
      val student2 = Student("ai2182", "Sanh", "Tsruw", "alb@mail.de", "44332211", UUID.randomUUID)
      val student3 = Student("mi3512", "Nahs", "Rustw", "lab@mail.de", "22331144", degree)
      val student4 = Student("mi5323", "Nahst", "Ruste", "lab@mail.de", "3213213", UUID.randomUUID)
      val student5 = Student("mi2453", "Nahss", "Kramral", "lab@mail.de", "2312", UUID.randomUUID)
      val student6 = Student("mi3452", "Nahsa", "Rusth", "lab@mail.de", "3213123", UUID.randomUUID)
      val employee1 = Employee("mlark", "Lars", "Marklar", "mark@mail.de", "lecturer")
      val employee2 = Employee("mlark", "Sarl", "Ralkram", "kram@mail.de", "employee")
      val employee3 = Employee("rlak", "Rasl", "Rustb", "ramk@mail.de", "lecturer")
      val users: Set[User] = Set(student1, student2, student3, student4, student5, student6, employee1, employee2, employee3)
      val lecturers: Set[Employee] = Set(employee1, employee3)
      val degreers: Set[Student] = Set(student1, student3)
      val firstnamers: Set[User] = Set(student3, student4, employee3, student6)
      val lastnamer: Set[Employee] = Set(employee2)
      val systemIders: Set[Student] = Set(student1, student2)
      val systemIder: Set[Student] = Set(student5)

      when(repository.get[User](anyObject(), anyObject())).thenReturn(Success(users))

      val request1 = FakeRequest(
        HttpVerbs.GET,
        s"/users?$statusAttribute=lecturer"
      )

      val request2 = FakeRequest(
        HttpVerbs.GET,
        s"/users?$degreeAttribute=" + degree
      )

      val request3 = FakeRequest(
        HttpVerbs.GET,
        s"/users?$firstnameAttribute=" + "rust"
      )

      val request4 = FakeRequest(
        HttpVerbs.GET,
        s"/users?$lastnameAttribute=" + "Sarl"
      )

      val request5 = FakeRequest(
        HttpVerbs.GET,
        s"/users?$systemIdAttribute=" + "ai"
      )

      val request6 = FakeRequest(
        HttpVerbs.GET,
        s"/users?$systemIdAttribute=" + "mi2453"
      )

      val result1 = controller.allUsers()(request1)
      val result2 = controller.allUsers()(request2)
      val result3 = controller.allUsers()(request3)
      val result4 = controller.allUsers()(request4)
      val result5 = controller.allUsers()(request5)
      val result6 = controller.allUsers()(request6)

      val jsonVals1 = lecturers map (l => Json.toJson(l))
      val jsonVals2 = degreers map (s => Json.toJson(s))
      val jsonVals3 = firstnamers map (u => Json.toJson(u))
      val jsonVals4 = lastnamer map (e => Json.toJson(e))
      val jsonVals5 = systemIders map (s => Json.toJson(s))
      val jsonVals6 = systemIder map (s => Json.toJson(s))

      status(result1) shouldBe OK
      val stringResult1 = contentAsString(result1)

      jsonVals1.forall { json =>
        stringResult1 contains json.toString
      } shouldBe true

      status(result2) shouldBe OK
      val stringResult2 = contentAsString(result2)

      jsonVals2.forall { json =>
        stringResult2 contains json.toString
      } shouldBe true

      status(result3) shouldBe OK
      val stringResult3 = contentAsString(result3)

      jsonVals3.forall { json =>
        stringResult3 contains json.toString
      } shouldBe true

      status(result4) shouldBe OK
      val stringResult4 = contentAsString(result4)

      jsonVals4.forall { json =>
        stringResult4 contains json.toString
      } shouldBe true

      status(result5) shouldBe OK
      val stringResult5 = contentAsString(result5)

      jsonVals5.forall { json =>
        stringResult5 contains json.toString
      } shouldBe true

      status(result6) shouldBe OK
      val stringResult6 = contentAsString(result6)

      jsonVals6.forall { json =>
        stringResult6 contains json.toString
      } shouldBe true
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

      when(repository.get[User](anyObject(), anyObject())).thenReturn(Success(users))
      when(repository.get[Degree](anyObject())(anyObject())).thenReturn(Success(Some(degree1)))

      val request1 = FakeRequest(
        HttpVerbs.GET,
        "/atomic/users?degree=" + degree1.id
      )

      val request2 = FakeRequest(
        HttpVerbs.GET,
        "/atomic/users?status=lecturer"
      )

      val result1 = controller.allUserAtomic()(request1)
      val result2 = controller.allUserAtomic()(request2)

      val jsonVals1 = jsonAtomic(degreers)
      val jsonVals2 = lecturers map (a => Json.toJson(a))
      val stringResult1 = contentAsString(result1)
      val stringResult2 = contentAsString(result2)

      status(result1) shouldBe OK
      jsonVals1.forall { json =>
        stringResult1 contains json.toString
      } shouldBe true

      status(result2) shouldBe OK
      jsonVals2.forall { json =>
        stringResult2 contains json.toString
      } shouldBe true
    }

    "successfully return requested buddy by his system id" in new FakeApp {
      val degree = UUID.randomUUID
      val currentUser = Student("systemId current", "last name current", "first name current", "email current", "regId current", degree)
      val buddy = Student("systemIdBuddy", "last name buddy", "first name buddy", "email buddy", "regId buddy", degree)

      when(repository.getMany[Student](anyObject())(anyObject())).thenReturn(Success(Set(currentUser, buddy)))
      when(repository.prepareQuery(anyObject())).thenReturn(query)
      when(qe.parse(anyObject())).thenReturn(sparqlOps.parseSelect("SELECT * where {}"))
      when(qe.execute(anyObject())).thenReturn(Success(Map.empty[String, List[Value]]))

      val request = FakeRequest(
        GET,
        s"/students/buddies/${buddy.systemId}"
      ).withSession(SessionController.userId -> currentUser.id.toString)

      val result = route(request).get

      status(result) shouldBe OK
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "OK",
        "id" -> buddy.id
      )
    }

    "not return requested buddy by his system id when degree doesn't match" in new FakeApp {
      val degree = UUID.randomUUID
      val currentUser = Student("systemId current", "last name current", "first name current", "email current", "regId current", degree)
      val buddy = Student("systemIdBuddy", "last name buddy", "first name buddy", "email buddy", "regId buddy", UUID.randomUUID)

      when(repository.getMany[Student](anyObject())(anyObject())).thenReturn(Success(Set(currentUser, buddy)))
      when(repository.prepareQuery(anyObject())).thenReturn(query)
      when(qe.parse(anyObject())).thenReturn(sparqlOps.parseSelect("SELECT * where {}"))
      when(qe.execute(anyObject())).thenReturn(Success(Map.empty[String, List[Value]]))

      val request = FakeRequest(
        GET,
        s"/students/buddies/${buddy.systemId}"
      ).withSession(SessionController.userId -> currentUser.id.toString)

      val result = route(request).get

      status(result) shouldBe BAD_REQUEST
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "message" -> "Students are not part of the same degree"
      )
    }

    "not return requested buddy when he is not found by his system id" in new FakeApp {
      val degree = UUID.randomUUID
      val currentUser = Student("systemId current", "last name current", "first name current", "email current", "regId current", degree)
      val buddy = Student("systemIdBuddy", "last name buddy", "first name buddy", "email buddy", "regId buddy", degree)

      when(repository.getMany[Student](anyObject())(anyObject())).thenReturn(Success(Set.empty[Student]))
      when(repository.prepareQuery(anyObject())).thenReturn(query)
      when(qe.parse(anyObject())).thenReturn(sparqlOps.parseSelect("SELECT * where {}"))
      when(qe.execute(anyObject())).thenReturn(Success(Map.empty[String, List[Value]]))

      val request = FakeRequest(
        GET,
        s"/students/buddies/${buddy.systemId}"
      ).withSession(SessionController.userId -> currentUser.id.toString)

      val result = route(request).get

      status(result) shouldBe NOT_FOUND
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "message" -> "No such element..."
      )
    }
  }
}