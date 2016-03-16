package bind.users

import base.SesameDbSpec
import models.users.{User, Employee}
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import store.Namespace
import store.bind.Bindings

import scala.util.{Failure, Success}

class EmployeeBindingSpec extends SesameDbSpec {
  import ops._
  implicit val ns = Namespace("http://lwm.gm.fh-koeln.de/")

  val bindings = Bindings[Sesame](ns)
  import bindings.uuidBinder
  import bindings.EmployeeBinding._

  val employee = Employee("doe", "Doe", "John", "doe@gm.fh-koeln.de", "employee")
  val employeeGraph = (
    URI(User.generateUri(employee)).a(lwm.User)
      -- lwm.systemId ->- employee.systemId
      -- lwm.lastname ->- employee.lastname
      -- lwm.firstname ->- employee.firstname
      -- lwm.email ->- employee.email
      -- lwm.status ->- employee.status
      -- lwm.id ->- employee.id
    ).graph

  "A EmployeeBinding" should {
    "return a RDF graph representation of an employee" in {
      val graph = employee.toPG.graph

      graph isIsomorphicWith employeeGraph shouldBe true
    }

    "return an employee based on a RDF graph representation" in {
      val expectedEmployee = PointedGraph[Rdf](URI(User.generateUri(employee)), employeeGraph).as[Employee]

      expectedEmployee match {
        case Success(s) =>
          s shouldEqual employee
        case Failure(e) =>
          fail(s"Unable to deserialise employee graph: $e")
      }
    }
  }

}