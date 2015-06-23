//package bind.schedule
//
//import base.SesameDbSpec
//import models.schedules.StudentSchedule
//import org.w3.banana.PointedGraph
//import org.w3.banana.sesame.Sesame
//import store.Namespace
//import store.bind.Bindings
//
//import scala.util.{Failure, Success}
//
//class StudentScheduleBindingSpec extends SesameDbSpec {
//  import ops._
//  implicit val ns = Namespace("http://lwm.gm.fh-koeln.de/")
//
//  val bindings = Bindings[Sesame](ns)
//  import bindings.uuidBinder
//  import bindings.StudentScheduleBinding._
//
//  val studentSchedule = StudentSchedule(StudentSchedule.randomUUID)
//  val studentScheduleGraph = (
//    URI(StudentSchedule.generateUri(studentSchedule)).a(lwm.StudentSchedule)
//      -- lwm.id ->- studentSchedule.id
//    ).graph
//
//  "A StudentScheduleBindingSpec" should {
//    "return a RDF graph representation of a studentSchedule" in {
//      val graph = studentSchedule.toPG.graph
//
//      graph isIsomorphicWith studentScheduleGraph shouldBe true
//    }
//    "return a studentSchedule based on a RDF graph representation" in {
//      val expectedStudentSchedule = PointedGraph[Rdf](URI(StudentSchedule.generateUri(studentSchedule)), studentScheduleGraph).as[StudentSchedule]
//
//      expectedStudentSchedule match {
//        case Success(s) =>
//          s shouldEqual studentSchedule
//        case Failure(e) =>
//          fail(s"Unable to deserialise studentSchedule graph: $e")
//      }
//    }
//    }
//}
