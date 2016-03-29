package controllers.reportCard

import java.util.UUID

import base.TestBaseDefinition
import models.labwork.{ReportCardEntryType, ReportCardEntry, ReportCard}
import org.joda.time.{LocalDate, LocalTime}
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.openrdf.model.impl.ValueFactoryImpl
import org.scalatest.WordSpec
import org.scalatest.mock.MockitoSugar.mock
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.SesameModule
import play.api.http.HeaderNames
import play.api.libs.json.Json
import play.api.test.Helpers._
import play.api.test.{FakeHeaders, FakeRequest}
import services.{RoleService, SessionHandlingService}
import store.{Namespace, SesameRepository}
import utils.LwmMimeType

import scala.util.{Failure, Success}

class ReportCardEntryTypeControllerSpec extends WordSpec with TestBaseDefinition with SesameModule {

  val repository = mock[SesameRepository]
  val roleService = mock[RoleService]
  val namespace = Namespace("http://lwm.gm.th-koeln.de")
  val sessionService = mock[SessionHandlingService]
  val factory = ValueFactoryImpl.getInstance()
  val mimeType = LwmMimeType.reportCardEntryTypeV1Json

  val reportCard = {
    val entries = Set(
      ReportCardEntry(0, "label", LocalDate.now, LocalTime.now, LocalTime.now, UUID.randomUUID, ReportCardEntryType.all)
    )
    ReportCard(UUID.randomUUID, UUID.randomUUID, entries)
  }
  val course = UUID.randomUUID.toString

  val controller: ReportCardEntryTypeController = new ReportCardEntryTypeController(repository, sessionService, namespace, roleService) {

    override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }

    override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }
  }
  "A ReportCardEntryTypeControllerSpec " should {

    "successfully update a report card entry type" in {
      import ReportCardEntryType._

      val entry = reportCard.entries.head
      val entryType = entry.entryTypes.head
      val toUpdate = ReportCardEntryType(entryType.entryType, !entryType.bool, entryType.int, entryType.id)

      when(repository.update(anyObject())(anyObject(), anyObject())).thenReturn(Success(PointedGraph[repository.Rdf](factory.createBNode(""))))

      val request = FakeRequest(
        PUT,
        s"/courses/$course/reportCards/${reportCard.id}/entries/${entry.id}/types/${entryType.id}",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> mimeType)),
        Json.toJson(toUpdate)
      )

      val result = controller.update(course, reportCard.id.toString, entry.id.toString, entryType.id.toString)(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(toUpdate)
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
        s"/courses/$course/reportCards/${reportCard.id}/entries/${entry.id}/types/${entryType.id}",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> mimeType)),
        brokenJson
      )

      val result = controller.update(course.toString, reportCard.id.toString, entry.id.toString, entryType.id.toString)(request)

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
        s"/courses/$course/reportCards/${reportCard.id}/entries/${entry.id}/types/${entryType.id}",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> mimeType)),
        Json.toJson(toUpdate)
      )

      val result = controller.update(course.toString, reportCard.id.toString, entry.id.toString, entryType.id.toString)(request)

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "errors" -> errorMessage
      )
    }
  }
}
