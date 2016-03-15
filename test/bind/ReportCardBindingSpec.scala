package bind

import java.util.UUID

import base.SesameDbSpec
import models.users.User
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
  import bindings.jodaLocalDateBinder
  import bindings.jodaLocalTimeBinder
  import bindings.uuidRefBinder
  import bindings.ReportCardBinding.reportCardBinder
  import bindings.ReportCardEntryBinding.reportCardEntryBinding
  import bindings.ReportCardEntryTypeBinding.reportCardEntryTypeBinding

  val entries = (0 until 5).map { n =>
    ReportCardEntry(n, n.toString, LocalDate.now.plusWeeks(n), LocalTime.now.plusHours(n), LocalTime.now.plusHours(n + 1), UUID.randomUUID(), ReportCardEntryType.all)
  }.toSet
  val reportCard = ReportCard(UUID.randomUUID(), UUID.randomUUID(), entries)
  val entry = entries.head
  val entryType = ReportCardEntryType.Attendance

  val reportCardGraph = (
    URI(ReportCard.generateUri(reportCard)).a(lwm.ReportCard)
      .--(lwm.student).->-(reportCard.student)(ops, uuidRefBinder(User.splitter))
      .--(lwm.labwork).->-(reportCard.labwork)(ops, uuidRefBinder(Labwork.splitter))
      -- lwm.entries ->- reportCard.entries
      -- lwm.id ->- reportCard.id
    ).graph

  val entryGraph = URI(ReportCardEntry.generateUri(entry)).a(lwm.ReportCardEntry)
    .--(lwm.index).->-(entry.index)
    .--(lwm.label).->-(entry.label)
    .--(lwm.date).->-(entry.date)
    .--(lwm.start).->-(entry.start)
    .--(lwm.end).->-(entry.end)
    .--(lwm.room).->-(entry.room)(ops, uuidRefBinder(Room.splitter))
    .--(lwm.types).->-(entry.entryTypes)
    .--(lwm.id).->-(entry.id).graph

  val typeGraph = (URI(ReportCardEntryType.generateUri(entryType)).a(lwm.ReportCardEntryType)
    -- lwm.entryType ->- entryType.entryType
    -- lwm.bool ->- entryType.bool
    -- lwm.int ->- entryType.int
    -- lwm.id ->- entryType.id
  ).graph

  "A ReportCardBindingSpec " should {

    "return a RDF graph representation of a report card" in {
      val graph = reportCard.toPG.graph

      graph isIsomorphicWith reportCardGraph shouldBe true
    }

    "return a report card based on a RDF graph representation" in {
      val expectedCard = PointedGraph[Rdf](URI(ReportCard.generateUri(reportCard)), reportCardGraph).as[ReportCard]

      expectedCard match {
        case Success(s) =>
          s shouldEqual reportCard
        case Failure(e) =>
          fail(s"Unable to deserialise report card graph: $e")
      }
    }

    "return a RDF graph representation of a report card entry" in {
      val graph = entry.toPG.graph

      graph isIsomorphicWith entryGraph shouldBe true
    }

    "return a report card entry based on a RDF graph representation" in {
      val expectedEntry = PointedGraph[Rdf](URI(ReportCardEntry.generateUri(entry)), entryGraph).as[ReportCardEntry]

      expectedEntry match {
        case Success(s) =>
          s shouldEqual entry
        case Failure(e) =>
          fail(s"Unable to deserialise report card entry graph: $e")
      }
    }

    "return a RDF graph representation of a report card entry type" in {
      val graph = entryType.toPG.graph

      graph isIsomorphicWith typeGraph shouldBe true
    }

    "return a report card entry type based on a RDF graph representation" in {
      val expectedType = PointedGraph[Rdf](URI(ReportCardEntryType.generateUri(entryType)), typeGraph).as[ReportCardEntryType]

      expectedType match {
        case Success(s) =>
          s shouldEqual entryType
        case Failure(e) =>
          fail(s"Unable to deserialise report card entry type graph: $e")
      }
    }
  }
}