package controllers

import java.util.UUID

import base.TestBaseDefinition
import models.{ReportCardEntryType, ReportCardEntry, ReportCard}
import org.joda.time.{LocalTime, LocalDate}
import org.openrdf.model.impl.ValueFactoryImpl
import org.scalatest.WordSpec
import org.w3.banana.PointedGraph
import play.api.http._
import play.api.libs.json.{JsError, Json}
import play.api.test.{FakeHeaders, FakeRequest}
import services.RoleService
import store.{Namespace, SesameRepository}
import org.mockito.Mockito._
import org.mockito.Matchers._
import org.scalatest.mock.MockitoSugar.mock
import play.api.test.Helpers._
import utils.LwmMimeType

import scala.util.{Failure, Success}

class ReportCardControllerSpec extends WordSpec with TestBaseDefinition{

  val repository = mock[SesameRepository]
  val roleService = mock[RoleService]
  val namespace = Namespace("http://lwm.gm.th-koeln.de")
  val factory = ValueFactoryImpl.getInstance()

  val reportCardController: ReportCardController = new ReportCardController(repository, namespace, roleService) {

    override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }

    override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }
  }

  val reportCard = {
    val entries = (0 until 5).map( n =>
      ReportCardEntry(n, n.toString, LocalDate.now.plusWeeks(n), LocalTime.now.plusHours(n), LocalTime.now.plusHours(n + 1), UUID.randomUUID(), ReportCardEntryType.all)
    ).toSet

    ReportCard(UUID.randomUUID(), UUID.randomUUID(), entries)
  }

  "A ReportCardControllerSpec " should {

    "successfully update a report card entry type" in {
      import ReportCardEntryType._

      val entry = reportCard.entries.head
      val entryType = entry.entryTypes.head
      val toUpdate = ReportCardEntryType(entryType.entryType, !entryType.bool, entryType.int, entryType.id)
      val course = UUID.randomUUID()

      when(repository.update(anyObject())(anyObject(), anyObject())).thenReturn(Success(PointedGraph[repository.Rdf](factory.createBNode(""))))

      val request = FakeRequest(
        PUT,
        s"/courses/$course/reportCards/${reportCard.id}/entries/${entry.id}",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> LwmMimeType.reportCardV1Json)),
        Json.toJson(toUpdate)
      )

      val result = reportCardController.updateReportCardEntryType(course.toString, reportCard.id.toString, entry.id.toString)(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "reportCardId" -> reportCard.id,
        "reportCardEntryId" -> entry.id,
        "assignmentEntryType" -> Json.toJson(toUpdate)
      )
    }

    "not update a report card entry type with invalid json data" in {
      val entry = reportCard.entries.head
      val entryType = entry.entryTypes.head
      val course = UUID.randomUUID()

      when(repository.update(anyObject())(anyObject(), anyObject())).thenReturn(Success(PointedGraph[repository.Rdf](factory.createBNode(""))))

      val brokenJson = Json.obj(
        "entryType" -> entryType.entryType,
        "id" -> entryType.id
      )
      val request = FakeRequest(
        PUT,
        s"/courses/$course/reportCards/${reportCard.id}/entries/${entry.id}",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> LwmMimeType.reportCardV1Json)),
        brokenJson
      )

      val result = reportCardController.updateReportCardEntryType(course.toString, reportCard.id.toString, entry.id.toString)(request)

      status(result) shouldBe BAD_REQUEST
      contentType(result) shouldBe Some("application/json")
    }

    "not update a report card entry type when there is an exception" in {
      val entry = reportCard.entries.head
      val entryType = entry.entryTypes.head
      val toUpdate = ReportCardEntryType(entryType.entryType, !entryType.bool, entryType.int, entryType.id)
      val course = UUID.randomUUID()
      val errorMessage = "Oops, something went wrong"

      when(repository.update(anyObject())(anyObject(), anyObject())).thenReturn(Failure(new Throwable(errorMessage)))

      val request = FakeRequest(
        PUT,
        s"/courses/$course/reportCards/${reportCard.id}/entries/${entry.id}",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> LwmMimeType.reportCardV1Json)),
        Json.toJson(toUpdate)
      )

      val result = reportCardController.updateReportCardEntryType(course.toString, reportCard.id.toString, entry.id.toString)(request)

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "errors" -> errorMessage
      )
    }
  }
}
