package bind.users

import base.SesameDbSpec
import models.users.Student
import store.Namespace
import store.bind.Bindings
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame

import scala.util.{Failure, Success}

class StudentBindingSpec extends SesameDbSpec {
  import ops._
  implicit val ns = Namespace("http://lwm.gm.fh-koeln.de/")

  val bindings = Bindings[Sesame](ns)
  import bindings.StudentBinding._
  import bindings.uuidBinder

  val student = Student("mi1234", "Doe", "John", "11234567", "mi1234@gm.fh-koeln.de")
  val studentGraph = (
    URI(Student.generateUri(student)).a(lwm.Student)
      -- lwm.systemId ->- student.systemId
      -- lwm.lastname ->- student.lastname
      -- lwm.firstname ->- student.firstname
      -- lwm.registrationId ->- student.registrationId
      -- lwm.email ->- student.email
      -- lwm.id ->- student.id
    ).graph

  "A StudentBinding" should {
    "return a RDF graph representation of a student" in {
      val graph = student.toPG.graph

      graph isIsomorphicWith studentGraph shouldBe true
    }

    "return a student based on a RDF graph representation" in {
      val expectedStudent = PointedGraph[Rdf](URI(Student.generateUri(student)), studentGraph).as[Student]

      expectedStudent match {
        case Success(s) =>
          s shouldEqual student
        case Failure(e) =>
          fail(s"Unable to deserialise student graph: $e")
      }
    }
  }
}
