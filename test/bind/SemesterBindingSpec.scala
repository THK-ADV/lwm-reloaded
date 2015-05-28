package bind

import base.SesameDbSpec
import models.Semester
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import store.Namespace
import store.bind.Bindings

import scala.util.{Failure, Success}

class SemesterBindingSpec extends SesameDbSpec {
  import ops._
  implicit val ns = Namespace("http://lwm.gm.fh-koeln.de/")

  val bindings = Bindings[Sesame](ns)
  import bindings._
  import bindings.SemesterBinding._

  val semester = Semester("name", "startDate", "endDate")
  val semesterGraph = (
    URI(Semester.generateUri(semester)).a(lwm.Semester)
      -- lwm.name ->- semester.name
      -- lwm.startDate ->- semester.startDate
      -- lwm.endDate ->- semester.endDate
      -- lwm.id ->- semester.id
    ).graph

  "A SemesterBindingSpec" should {
    "return a RDF graph representation of a semester" in {
      val graph = semester.toPG.graph

      graph isIsomorphicWith semesterGraph shouldBe true
    }
    "return a semester based on a RDF graph representation" in {
      val expectedSemester = PointedGraph[Rdf](URI(Semester.generateUri(semester)), semesterGraph).as[Semester]

      expectedSemester match {
        case Success(s) =>
          s shouldEqual semester
        case Failure(e) =>
          fail(s"Unable to deserialise semester graph: $e")
      }
    }
    }
}
