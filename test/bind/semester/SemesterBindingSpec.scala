package bind.semester

import base.SesameDbSpec
import models.semester.{Blacklist, Semester}
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import store.Namespace
import store.bind.Bindings

import scala.util.{Failure, Success}

class SemesterBindingSpec extends SesameDbSpec {
  import ops._
  implicit val ns = Namespace("http://lwm.gm.fh-koeln.de/")

  val bindings = Bindings[Sesame](ns)
  import bindings.uuidBinder
  import bindings.BlacklistBinding.blacklistBinder
  import bindings.SemesterBinding.semesterBinder

  val semester = Semester("name", "startDate", "endDate", "examPeriod", Blacklist.empty, Semester.randomUUID)
  val semesterGraph = (
    URI(Semester.generateUri(semester)).a(lwm.Semester)
      -- lwm.name ->- semester.name
      -- lwm.start ->- semester.startDate
      -- lwm.end ->- semester.endDate
      -- lwm.exam ->- semester.examPeriod
      -- lwm.blacklist ->- semester.blacklist
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
