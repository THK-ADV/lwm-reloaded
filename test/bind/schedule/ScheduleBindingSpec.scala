package bind.schedule

import base.SesameDbSpec
import models.Labwork
import models.schedule.Schedule
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import store.Namespace
import store.bind.Bindings
import scala.util.{Success, Failure}

class ScheduleBindingSpec extends SesameDbSpec {
  implicit val ns = Namespace("http://lwm.gm.fh-koeln.de/")

  val bindings = Bindings[Sesame](ns)

  import ops._
  import bindings.uuidBinder
  import bindings.ScheduleBinding.scheduleBinder

  val schedule = Schedule(Labwork.randomUUID, Schedule.randomUUID)
  val scheduleGraph = (
    URI(Schedule.generateUri(schedule)).a(lwm.Schedule)
      -- lwm.labwork ->- schedule.labwork
      -- lwm.id ->- schedule.id
    ).graph

  "A ScheduleBindingSpec" should {
    "return a RDF graph representation of a schedule" in {
      val graph = schedule.toPG.graph

      graph isIsomorphicWith scheduleGraph shouldBe true
    }

    "return a schedule based on a RDF graph representation" in {
      val expectedTimetable = PointedGraph[Rdf](URI(Schedule.generateUri(schedule)), scheduleGraph).as[Schedule]

      expectedTimetable match {
        case Success(s) =>
          s shouldEqual schedule
        case Failure(e) =>
          fail(s"Unable to deserialise schedule graph: $e")
      }
    }
  }
}
