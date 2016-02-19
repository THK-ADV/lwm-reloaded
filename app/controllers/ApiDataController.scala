package controllers

import models.{Degree, Room}
import models.security.{RefRole, Roles, Authority, Role}
import models.security.Roles._
import models.users.Employee
import org.w3.banana.PointedGraph
import play.api.libs.json.{JsArray, Json}
import play.api.mvc.{Action, Controller}
import store.SesameRepository
import store.bind.Bindings
import scala.util.Random._
import scalaz.syntax.monad._
import scala.util.{Failure, Success, Try}

class ApiDataController(val repository: SesameRepository) extends Controller {
  import repository.ops
  private val bindings = Bindings(repository.namespace)

  def populate = Action { request =>
    import bindings.RoleBinding._
    import bindings.RoomBinding._
    import bindings.EmployeeBinding._
    import bindings.AuthorityBinding._
    import bindings.DegreeBinding._

    def roomgen(n: Int) = Stream.continually(Room(s"R ${nextInt(3)}.${nextInt(9)}${nextInt(9)}${nextInt(9)}", "Desc")).take(n) ++ List(Room("H32-LC", "H32-LC Desc"), Room("H32-BG", "H32-BG Desc"), Room("H32-HA", "H32-HA Desc"))

    val rooms = roomgen(10).toList.map(repository.add[Room])
    val roles = List(admin, student, employee, user).map(repository.add[Role])
    val people = List(Employee("ai1818", "Wurst", "Hans", "", Employee.randomUUID))
    val authorities = people.map(p => (p, Authority(p.id, Set(RefRole(None, Roles.admin.id))))).foldLeft(List[Try[PointedGraph[repository.Rdf]]]()) {
      case (l, (emp, auth)) => l :+ repository.add[Employee](emp) :+ repository.add[Authority](auth)
    }
    val degrees = List(Degree("Allgemeine Informatik", "AI", Degree.randomUUID), Degree("Medieninformatik", "MI", Degree.randomUUID), Degree("Technische Informatik", "TI", Degree.randomUUID), Degree("Wirtschaftsinformatik", "WI", Degree.randomUUID))
      .map(repository.add[Degree])
    (rooms ++ roles ++ authorities ++ degrees).foldRight(Try(List[PointedGraph[repository.Rdf]]())) { (l, r) =>
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
    (for {
      roles <- repository.get[Role](RoleBinding.roleBinder, RoleBinding.classUri)
      rooms <- repository.get[Room](RoomBinding.roomBinder, RoomBinding.classUri)
      people <- repository.get[Employee](EmployeeBinding.employeeBinder, EmployeeBinding.classUri)
      auths <- repository.get[Authority](AuthorityBinding.authorityBinder, AuthorityBinding.classUri)
      degrees <- repository.get[Degree](DegreeBinding.degreeBinder, DegreeBinding.classUri)
    } yield {
      List(
        Json.toJson(roles),
        Json.toJson(rooms),
        Json.toJson(people),
        Json.toJson(auths),
        Json.toJson(degrees)
      )
    }) match {
      case Success(json) => Ok(json.foldLeft(JsArray())((l, r) => l ++ r.asInstanceOf[JsArray]))
      case Failure(e) => InternalServerError(e.getMessage)
    }
  }
}