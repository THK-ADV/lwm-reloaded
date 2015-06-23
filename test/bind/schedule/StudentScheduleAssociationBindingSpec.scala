package bind.schedule

import base.SesameDbSpec
import models.schedules.StudentScheduleAssociation
import store.Namespace
import store.bind.Bindings
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame

import scala.util.{Failure, Success}

class StudentScheduleAssociationBindingSpec extends SesameDbSpec {
  import ops._
  implicit val ns = Namespace("http://lwm.gm.fh-koeln.de/")

  val bindings = Bindings[Sesame](ns)
  import bindings.StudentScheduleAssociationBinding._
  import bindings.uuidBinder

  val studentScheduleAssociation = StudentScheduleAssociation("date", "groupScheduleAssociation", "timetableEntry", StudentScheduleAssociation.randomUUID)
  val studentScheduleAssociationGraph = (
    URI(StudentScheduleAssociation.generateUri(studentScheduleAssociation)).a(lwm.StudentScheduleAssociation)
      -- lwm.date ->- studentScheduleAssociation.date
      -- lwm.groupScheduleAssociation ->- studentScheduleAssociation.groupScheduleAssociation
      -- lwm.timetableEntry ->- studentScheduleAssociation.timetableEntry
      -- lwm.id ->- studentScheduleAssociation.id
    ).graph

  "A StudentScheduleAssociationBindingSpec" should {
    "return a RDF graph representation of a studentScheduleAssociation" in {
      val graph = studentScheduleAssociation.toPG.graph

      graph isIsomorphicWith studentScheduleAssociationGraph shouldBe true
    }
      "return a studentScheduleAssociation based on a RDF graph representation" in {
        val expectedStudentScheduleAssociation = PointedGraph[Rdf](URI(StudentScheduleAssociation.generateUri(studentScheduleAssociation)), studentScheduleAssociationGraph).as[StudentScheduleAssociation]

        expectedStudentScheduleAssociation match {
          case Success(s) =>
            s shouldEqual studentScheduleAssociation
          case Failure(e) =>
            fail(s"Unable to deserialise studentScheduleAssociation graph: $e")
        }
      }
    }
}
