package controllers

import java.util.UUID

import models.{Course, Degree, Labwork, Room}
import models.security._
import models.security.Roles._
import models.semester.Semester
import models.users.{Employee, Student, StudentProtocol, User}
import org.joda.time.LocalDate
import org.w3.banana.PointedGraph
import play.api.libs.json.{JsArray, Json}
import play.api.mvc.{Action, Controller}
import store.SesameRepository
import store.bind.Bindings

import scala.language.implicitConversions
import scala.util.Random._
import scala.util.{Failure, Success, Try}


class ApiDataController(val repository: SesameRepository) extends Controller {
  import repository.ops
  private val bindings = Bindings(repository.namespace)
  implicit def toLocalDate(s: String): LocalDate = LocalDate.parse(s)

  val uuid = UUID.randomUUID

  def deleteShit() = Action { request =>
    val uri = s"${repository.namespace}/students/$uuid"
    println(uri)
    repository.delete(uri) match {
      case Success(s) => Ok("YAY")
      case Failure(_) => InternalServerError("Du AFFE")
    }
  }

  def populate = Action { request =>
    (rooms(10) ++
      roles ++
      authorities ++
      degrees ++
      semesters ++
      courses ++
      refroles).foldRight(Try(List[PointedGraph[repository.Rdf]]())) { (l, r) =>
      l match {
        case Success(g) => r map (_ :+ g)
        case Failure(e) => Failure(e)
      }
    } match {
      case Success(g) => Ok("Graph created")
      case Failure(e) => InternalServerError(e.getMessage)
    }
  }

  def getAdded = Action { request =>
    import bindings.RoleBinding
    import bindings.RoomBinding
    import bindings.EmployeeBinding
    import bindings.AuthorityBinding
    import bindings.DegreeBinding
    import bindings.SemesterBinding
    import bindings.CourseBinding
    import bindings.RefRoleBinding
    import bindings.StudentBinding
    (for {
      roles <- repository.get[Role](RoleBinding.roleBinder, RoleBinding.classUri)
      rooms <- repository.get[Room](RoomBinding.roomBinder, RoomBinding.classUri)
      people <- repository.get[Employee](EmployeeBinding.employeeBinder, EmployeeBinding.classUri)
      auths <- repository.get[Authority](AuthorityBinding.authorityBinder, AuthorityBinding.classUri)
      degrees <- repository.get[Degree](DegreeBinding.degreeBinder, DegreeBinding.classUri)
      semesters <- repository.get[Semester](SemesterBinding.semesterBinder, SemesterBinding.classUri)
      courses <- repository.get[Course](CourseBinding.courseBinder, CourseBinding.classUri)
      refrole <- repository.get[RefRole](RefRoleBinding.refRoleBinder, RefRoleBinding.classUri)
    } yield {
      List(
        Json.toJson(roles),
        Json.toJson(refrole),
        Json.toJson(auths),
        Json.toJson(rooms),
        Json.toJson(people),
        Json.toJson(degrees),
        Json.toJson(semesters),
        Json.toJson(courses)
      )
    }) match {
      case Success(json) => Ok(json.foldLeft(JsArray())((l, r) => l ++ r.asInstanceOf[JsArray]))
      case Failure(e) => InternalServerError(e.getMessage)
    }
  }


  val studentList = List(Student("mi1111", "Hans", "Muruk", "", "11992200", UUID.randomUUID(), uuid))

  val studentProts = studentList map { s =>
    StudentProtocol(s.systemId, s.lastname, s.firstname, s.email, s.registrationId)
  }

  def rooms(n: Int) = {
    import bindings.RoomBinding._
    def roomgen(n: Int) = Stream.continually(Room(s"R ${nextInt(3)}.${nextInt(9)}${nextInt(9)}${nextInt(9)}", "Desc")).take(n) ++ List(Room("H32-LC", "H32-LC Desc"), Room("H32-BG", "H32-BG Desc"), Room("H32-HA", "H32-HA Desc"))
    roomgen(n) map repository.add[Room]
  }

  def roles = {
    import bindings.RoleBinding._
    List(admin /*student, employee, user*/) map repository.add[Role]
}

  def people = List(Employee("ai1818", "Wurst", "Hans", "", Employee.randomUUID))

  def authorities = {
    import bindings.AuthorityBinding._
    import bindings.EmployeeBinding._

    people.map(p => (p, Authority(p.id, Set(refrole1.id)))).foldLeft(List[Try[PointedGraph[repository.Rdf]]]()) {
      case (l, (emp, auth)) => l :+ repository.add[Employee](emp) :+ repository.add[Authority](auth)
    }
  }

  def refroles = {
    import bindings.RefRoleBinding._
    List(refrole1) map repository.add[RefRole]
  }

  def degrees = {
    import bindings.DegreeBinding._
    List(
      Degree("Allgemeine Informatik", "AI", Degree.randomUUID),
      Degree("Medieninformatik", "MI", Degree.randomUUID),
      Degree("Technische Informatik", "TI", Degree.randomUUID),
      Degree("Wirtschaftsinformatik", "WI", Degree.randomUUID))
      .map(repository.add[Degree])
  }

  def semesters = {
    import bindings.SemesterBinding._

    List(
      Semester("Sommersemester 2015", "SS 15", "2015-03-01", "2015-08-31", "2015-07-11", Semester.randomUUID),
      Semester("Wintersemester 2015/2016", "WS 15/16", "2015-09-01", "2016-02-29", "2016-02-01", Semester.randomUUID),
      Semester("Sommersemester 2016", "SS 16", "2016-03-01", "2016-08-31", "2016-07-11", Semester.randomUUID))
      .map(repository.add[Semester])
  }

  def courses = {
    import bindings.CourseBinding._

    List(
      Course("Mathematik 1", "Konen", "MA 1", User.randomUUID, 1, Course.randomUUID),
      Course("Mathematik 1", "Leopold", "MA 1", User.randomUUID, 1, Course.randomUUID),
      Course("Mathematik 2", "Schmitter", "MA 2", User.randomUUID, 2, Course.randomUUID),
      Course("Algorithmen und Programmierung 1", "Victor", "AP 1", User.randomUUID, 1, Course.randomUUID),
      Course("Computergrafik und Animation", "Eisemann", "CGA", User.randomUUID, 3, Course.randomUUID),
      Course("Algorithmen und Programmierung 2", "Kohls", "AP 2", User.randomUUID, 2, Course.randomUUID))
      .map(repository.add[Course])
  }
}