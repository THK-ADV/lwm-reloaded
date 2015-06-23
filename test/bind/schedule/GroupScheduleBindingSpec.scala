//package bind.schedule
//
//import base.SesameDbSpec
//import models.schedules.GroupSchedule
//import store.Namespace
//import store.bind.Bindings
//import org.w3.banana.PointedGraph
//import org.w3.banana.sesame.Sesame
//
//import scala.util.{Failure, Success}
//
//class GroupScheduleBindingSpec extends SesameDbSpec {
//  import ops._
//  implicit val ns = Namespace("http://lwm.gm.fh-koeln.de/")
//
//  val bindings = Bindings[Sesame](ns)
//  import bindings.GroupScheduleBinding._
//  import bindings.uuidBinder
//
//  val groupSchedule = GroupSchedule(GroupSchedule.randomUUID)
//  val groupScheduleGraph = (
//    URI(GroupSchedule.generateUri(groupSchedule)).a(lwm.GroupSchedule)
//      -- lwm.id ->- groupSchedule.id
//    ).graph
//
//  "A GroupScheduleBindingSpec" should {
//    "return a RDF graph representation of a groupSchedule" in {
//      val graph = groupSchedule.toPG.graph
//
//      graph isIsomorphicWith groupScheduleGraph shouldBe true
//    }
//    "return a groupSchedule based on a RDF graph representation" in {
//      val expectedGroupSchedule = PointedGraph[Rdf](URI(GroupSchedule.generateUri(groupSchedule)), groupScheduleGraph).as[GroupSchedule]
//
//      expectedGroupSchedule match {
//        case Success(s) =>
//          s shouldEqual groupSchedule
//        case Failure(e) =>
//          fail(s"Unable to deserialise groupSchedule graph: $e")
//      }
//    }
//    }
//}
