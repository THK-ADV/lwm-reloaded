package bind

import base.SesameDbSpec
import models.semester.Semester
import org.joda.time.LocalDate
import org.w3.banana.PointedGraph

import scala.util.{Failure, Success}

class SemesterBindingSpec extends SesameDbSpec {

  import bindings.{SemesterDescriptor, dateTimeBinder, localDateBinder, uuidBinder}
  import ops._

  implicit val semesterBinder = SemesterDescriptor.binder

  val semester = Semester("label", "abbreviation", LocalDate.now, LocalDate.now.plusMonths(6), LocalDate.now.plusMonths(5))
  val semesterGraph = (
    URI(Semester.generateUri(semester)).a(lwm.Semester)
      -- lwm.label ->- semester.label
      -- lwm.abbreviation ->- semester.abbreviation
      -- lwm.start ->- semester.start
      -- lwm.end->- semester.end
      -- lwm.examStart ->- semester.examStart
      -- lwm.invalidated ->- semester.invalidated
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
