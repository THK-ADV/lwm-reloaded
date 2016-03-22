package controllers.reportCard

import java.util.UUID

import base.TestBaseDefinition
import models.{ReportCard, ReportCardEntry, ReportCardEntryType, Rescheduled}
import org.joda.time.{LocalDate, LocalTime}
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.openrdf.model.impl.ValueFactoryImpl
import org.scalatest.WordSpec
import org.scalatest.mock.MockitoSugar.mock
import org.w3.banana.PointedGraph
import play.api.http.HeaderNames
import play.api.libs.json.Json
import play.api.test.Helpers._
import play.api.test.{FakeHeaders, FakeRequest}
import services.{RoleService, SessionHandlingService}
import store.{Namespace, SesameRepository}
import utils.LwmMimeType

import scala.util.{Failure, Success}

class ReportCardEntryControllerSpec extends WordSpec with TestBaseDefinition {

  val repository = mock[SesameRepository]
  val roleService = mock[RoleService]
  val namespace = Namespace("http://lwm.gm.th-koeln.de")
  val sessionService = mock[SessionHandlingService]
  val factory = ValueFactoryImpl.getInstance()

  val mimeType = LwmMimeType.reportCardEntryV1Json
  val reportCard = {
    val entries = (0 until 5).map( n =>
      ReportCardEntry(n, n.toString, LocalDate.now.plusWeeks(n), LocalTime.now.plusHours(n), LocalTime.now.plusHours(n + 1), UUID.randomUUID, ReportCardEntryType.all)
    ).toSet

    ReportCard(UUID.randomUUID, UUID.randomUUID, entries)
  }
  val entry = reportCard.entries.head
  val course = UUID.randomUUID.toString

  val controller: ReportCardEntryController = new ReportCardEntryController(repository, sessionService, namespace, roleService) {

    override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }

    override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }
  }

  "A ReportCardEntryControllerSpec " should {

    "successfully reschedule a student's report card entry" in {
      import ReportCardEntry.writes

      val rescheduledEntry = {
        val rescheduled = Rescheduled(entry.date.plusDays(3), entry.start.plusHours(1), entry.end.plusHours(1), UUID.randomUUID)
        ReportCardEntry(entry.index, entry.label, entry.date, entry.start, entry.end, entry.room, entry.entryTypes, Some(rescheduled), entry.id)
      }

      when(repository.update(anyObject())(anyObject(), anyObject())).thenReturn(Success(PointedGraph[repository.Rdf](factory.createBNode(""))))

      val request = FakeRequest(
        GET,
        s"/courses/$course/reportCards/${reportCard.id}/entries/${entry.id}",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> mimeType)),
        Json.toJson(rescheduledEntry)
      )
      val result = controller.update(course, reportCard.id.toString, entry.id.toString)(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(rescheduledEntry)
    }

    "not update a report card entry type with invalid json data" in {
      val invalidJson = Json.obj("first" -> 0)
      val request = FakeRequest(
        GET,
        s"/courses/$course/reportCards/${reportCard.id}/entries/${entry.id}",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> mimeType)),
        invalidJson
      )
      val result = controller.update(course, reportCard.id.toString, entry.id.toString)(request)

      status(result) shouldBe BAD_REQUEST
      contentType(result) shouldBe Some("application/json")
    }

    "not update a report card entry type when there is an exception" in {
      val errorMessage = "Oops, something went wrong"
      val rescheduledEntry = {
        val rescheduled = Rescheduled(entry.date.plusDays(3), entry.start.plusHours(1), entry.end.plusHours(1), UUID.randomUUID)
        ReportCardEntry(entry.index, entry.label, entry.date, entry.start, entry.end, entry.room, entry.entryTypes, Some(rescheduled), entry.id)
      }

      when(repository.update(anyObject())(anyObject(), anyObject())).thenReturn(Failure(new Throwable(errorMessage)))

      val request = FakeRequest(
        GET,
        s"/courses/$course/reportCards/${reportCard.id}/entries/${entry.id}",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> mimeType)),
        Json.toJson(rescheduledEntry)
      )
      val result = controller.update(course, reportCard.id.toString, entry.id.toString)(request)

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "errors" -> errorMessage
      )
    }
  }
}
