package bind

import java.util.UUID

import base.SesameDbSpec
import models.users.{User, Student}
import models._
import org.joda.time.{LocalTime, LocalDate}
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import store.Namespace
import store.bind.Bindings

import scala.util.{Failure, Success}

class ReportCardBindingSpec extends SesameDbSpec {

  import ops._
  implicit val ns = Namespace("http://lwm.gm.fh-koeln.de/")

  val bindings = Bindings[Sesame](ns)
  import bindings.uuidBinder
  import bindings.uuidRefBinder
  import bindings.ReportCardBinding.reportCardBinder
  import bindings.ReportCardEntryBinding.reportCardEntryBinding

  val entries = (0 until 5).map { n =>
    import models.AssignmentEntryType._
    ReportCardEntry(n, n.toString, LocalDate.now.plusWeeks(n), LocalTime.now.plusHours(n), LocalTime.now.plusHours(n + 1), UUID.randomUUID(), AssignmentEntryType.all.toSet.map(fromProtocol))
  }.toSet
  val reportCard = ReportCard(UUID.randomUUID(), UUID.randomUUID(), entries)
  val reportCardGraph = (
    URI(ReportCard.generateUri(reportCard)).a(lwm.ReportCard)
      .--(lwm.student).->-(reportCard.student)(ops, uuidRefBinder(User.splitter))
      .--(lwm.labwork).->-(reportCard.labwork)(ops, uuidRefBinder(Labwork.splitter))
      -- lwm.entries ->- reportCard.entries
      -- lwm.id ->- reportCard.id
    ).graph

  "A ReportCardBindingSpec" should {
    "return a RDF graph representation of a report card" in {
      val graph = reportCard.toPG.graph

      graph isIsomorphicWith reportCardGraph shouldBe true
    }

    "return a report card based on a RDF graph representation" in {
      val expectedRoom = PointedGraph[Rdf](URI(ReportCard.generateUri(reportCard)), reportCardGraph).as[ReportCard]

      expectedRoom match {
        case Success(s) =>
          s shouldEqual reportCard
        case Failure(e) =>
          fail(s"Unable to deserialise room graph: $e")
      }
    }
  }
}