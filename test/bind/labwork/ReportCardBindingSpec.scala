package bind.labwork

import java.util.UUID

import base.SesameDbSpec
import models._
import models.labwork._
import models.users.User
import org.joda.time.{LocalDate, LocalTime}
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import store.bind.Bindings

import scala.util.{Failure, Success}

class ReportCardBindingSpec extends SesameDbSpec {

  import ops._

  val bindings = Bindings[Sesame](namespace)
  import bindings.ReportCardBinding.reportCardBinder
  import bindings.ReportCardEntryBinding.reportCardEntryBinding
  import bindings.ReportCardEntryTypeBinding.reportCardEntryTypeBinding
  import bindings.{jodaLocalDateBinder, jodaLocalTimeBinder, uuidBinder, uuidRefBinder}

  val entries = (0 until 5).map { n =>
    ReportCardEntry(n, n.toString, LocalDate.now.plusWeeks(n), LocalTime.now.plusHours(n), LocalTime.now.plusHours(n + 1), UUID.randomUUID(), ReportCardEntryType.all)
  }.toSet
  val reportCard = ReportCard(UUID.randomUUID(), UUID.randomUUID(), entries)
  val reportCardEntry = {
    val first = entries.head
    val rescheduled = Rescheduled(LocalDate.now, LocalTime.now, LocalTime.now, UUID.randomUUID)
    ReportCardEntry(first.index, first.label, first.date, first.start, first.end, first.room, first.entryTypes, Some(rescheduled), first.id)
  }
  val entryType = ReportCardEntryType.Attendance

  val reportCardGraph = (
    URI(ReportCard.generateUri(reportCard)).a(lwm.ReportCard)
      .--(lwm.student).->-(reportCard.student)(ops, uuidRefBinder(User.splitter))
      .--(lwm.labwork).->-(reportCard.labwork)(ops, uuidRefBinder(Labwork.splitter))
      -- lwm.entries ->- reportCard.entries
      -- lwm.id ->- reportCard.id
    ).graph

  val entryGraph = URI(ReportCardEntry.generateUri(reportCardEntry)).a(lwm.ReportCardEntry)
    .--(lwm.index).->-(reportCardEntry.index)
    .--(lwm.label).->-(reportCardEntry.label)
    .--(lwm.date).->-(reportCardEntry.date)
    .--(lwm.start).->-(reportCardEntry.start)
    .--(lwm.end).->-(reportCardEntry.end)
    .--(lwm.room).->-(reportCardEntry.room)(ops, uuidRefBinder(Room.splitter))
    .--(lwm.types).->-(reportCardEntry.entryTypes)
    .--(lwm.rescheduled).->-(reportCardEntry.entryTypes)
    .--(lwm.id).->-(reportCardEntry.id).graph

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

    "successfully serialise a report card entry" in {
      val entry = reportCardEntryBinding.fromPG(reportCardEntry.toPG)

      entry shouldBe Success(reportCardEntry)
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