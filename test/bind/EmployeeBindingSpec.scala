package bind

import base.SesameDbSpec
import models.{SesameEmployee, User}
import org.w3.banana.PointedGraph

import scala.util.{Failure, Success}

class EmployeeBindingSpec extends SesameDbSpec {

  import bindings.{EmployeeDescriptor, dateTimeBinder, uuidBinder}
  import ops._

  implicit val employeeBinder = EmployeeDescriptor.binder

  val employee = SesameEmployee("doe", "Doe", "John", "doe@gm.fh-koeln.de", "employee")
  val employeeGraph = (
    URI(User.generateUri(employee)).a(lwm.User)
      -- lwm.systemId ->- employee.systemId
      -- lwm.lastname ->- employee.lastname
      -- lwm.firstname ->- employee.firstname
      -- lwm.email ->- employee.email
      -- lwm.status ->- employee.status
      -- lwm.invalidated ->- employee.invalidated
      -- lwm.id ->- employee.id
    ).graph

  "A EmployeeBinding" should {
    "return a RDF graph representation of an employee" in {
      val graph = employee.toPG.graph

      graph isIsomorphicWith employeeGraph shouldBe true
    }

    "return an employee based on a RDF graph representation" in {
      val expectedEmployee = PointedGraph[Rdf](URI(User.generateUri(employee)), employeeGraph).as[SesameEmployee]

      expectedEmployee match {
        case Success(s) =>
          s shouldEqual employee
        case Failure(e) =>
          fail(s"Unable to deserialise employee graph: $e")
      }
    }
  }

}