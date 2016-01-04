package controllers

import models.Room
import models.security.Role
import models.security.Roles._
import org.w3.banana.PointedGraph
import play.api.libs.json._
import play.api.mvc.{Action, Controller}
import store.SesameRepository
import store.bind.Bindings
import scala.language.higherKinds
import scala.util.{Failure, Success, Try}
import scala.util.Random._

class ApiDataController(val repository: SesameRepository) extends Controller {
  import repository.ops
  private val bindings = Bindings(repository.namespace)

  def populate = Action { request =>
    import bindings.RoleBinding._
    import bindings.RoomBinding._
    def roomgen(n: Int) = Stream.continually(Room(s"R ${nextInt(3)}.${nextInt(9)}${nextInt(9)}${nextInt(9)}")).take(n) ++ List(Room("H32-LC"), Room("H32-BG"), Room("H32-HA"))

    val rooms = roomgen(10).toList.map(repository.add[Room])
    val roles = List(admin, student, employee, user).map(repository.add[Role])

    (rooms ++ roles).foldRight(Try(List[PointedGraph[repository.Rdf]]())) { (l, r) =>
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
    (for {
      roles <- repository.get[Role](RoleBinding.roleBinder, RoleBinding.classUri)
      rooms <- repository.get[Room](RoomBinding.roomBinder, RoomBinding.classUri)
    } yield {
      List(
        Json.toJson(roles),
        Json.toJson(rooms)
      )
    }) match {
      case Success(json) => Ok(json.foldLeft(JsArray())((l, r) => l ++ r.asInstanceOf[JsArray]))
      case Failure(e) => InternalServerError(e.getMessage)
    }
  }

}
