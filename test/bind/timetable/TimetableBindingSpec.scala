//package bind.timetable
//
//import base.SesameDbSpec
//import models.timetable.Timetable
//import org.w3.banana.PointedGraph
//import org.w3.banana.sesame.Sesame
//import store.Namespace
//import store.bind.Bindings
//
//import scala.util.{Failure, Success}
//
//class TimetableBindingSpec extends SesameDbSpec {
//  import ops._
//  implicit val ns = Namespace("http://lwm.gm.fh-koeln.de/")
//
//  val bindings = Bindings[Sesame](ns)
//  import bindings.TimetableBinding._
//  import bindings.uuidBinder
//
//  val timetable = Timetable(Timetable.randomUUID)
//  val timetableGraph = (
//    URI(Timetable.generateUri(timetable)).a(lwm.Timetable)
//      -- lwm.id ->- timetable.id
//    ).graph
//
//  "A TimetableBindingSpec" should {
//    "return a RDF graph representation of a timetable" in {
//      val graph = timetable.toPG.graph
//
//      graph isIsomorphicWith timetableGraph shouldBe true
//    }
//    "return a timetable based on a RDF graph representation" in {
//      val expectedTimetable = PointedGraph[Rdf](URI(Timetable.generateUri(timetable)), timetableGraph).as[Timetable]
//
//      expectedTimetable match {
//        case Success(s) =>
//          s shouldEqual timetable
//        case Failure(e) =>
//          fail(s"Unable to deserialise timetable graph: $e")
//      }
//    }
//    }
//}
