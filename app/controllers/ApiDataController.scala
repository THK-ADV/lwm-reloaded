package controllers

import models.security.Role
import models.security.Roles._
import org.w3.banana.PointedGraph
import play.api.libs.json.Json
import play.api.mvc.{Action, Controller}
import store.{SesameRepository, SemanticRepository}
import store.bind.Bindings

import scala.util.{Failure, Success, Try}

class ApiDataController(val repository: SesameRepository) extends Controller {
  import repository.ops
  private val bindings = Bindings(repository.namespace)

  def populate = Action { request =>
    import bindings.RoleBinding._
    List(admin, student, employee).map(repository.add[Role]).foldRight(Try(List[PointedGraph[repository.Rdf]]())) { (l, r) =>
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
    import bindings.RoleBinding._
    repository.get[Role] match {
      case Success(roles) => Ok(Json.toJson(roles))
      case Failure(e) => InternalServerError(e.getMessage)
    }
  }

}
