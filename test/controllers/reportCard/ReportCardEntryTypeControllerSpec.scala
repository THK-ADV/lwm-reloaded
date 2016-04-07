package controllers.reportCard

import java.util.UUID

import base.TestBaseDefinition
import models.labwork.{ReportCardEntry, ReportCardEntryType}
import org.joda.time.{LocalTime, LocalDate}
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.openrdf.model.Value
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
import store.sparql.{QueryEngine, SelectClause, QueryExecutor}
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
  val qe = mock[QueryExecutor[SelectClause]]
  val query = QueryEngine.empty(qe)

  val types = ReportCardEntryType.all
  val entry = UUID.randomUUID
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

      val entryType = types.head
      val toUpdate = ReportCardEntryType(entryType.entryType, !entryType.bool, entryType.int, entryType.id)

      when(repository.update(anyObject())(anyObject(), anyObject())).thenReturn(Success(PointedGraph[repository.Rdf](factory.createBNode(""))))

      val request = FakeRequest(
        PUT,
        s"/courses/$course/reportCardEntries/$entry/types/${entryType.id}",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> mimeType)),
        Json.toJson(toUpdate)
      )

      val result = controller.update(course, entry.toString, entryType.id.toString)(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(toUpdate)
    }

    "not update when there is an inconsistency" in {
      val entryType = types.head
      val toUpdate = ReportCardEntryType(entryType.entryType, !entryType.bool, entryType.int, entryType.id)

      val request = FakeRequest(
        PUT,
        s"/courses/$course/reportCardEntries/$entry/types/${entryType.id}",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> mimeType)),
        Json.toJson(toUpdate)
      )

      val result = controller.update(course, entry.toString, UUID.randomUUID.toString)(request)

      status(result) shouldBe BAD_REQUEST
      contentType(result) shouldBe Some("application/json")
    }

    "not update a report card entry type with invalid json data" in {
      val entryType = types.head
      val course = UUID.randomUUID()

      when(repository.update(anyObject())(anyObject(), anyObject())).thenReturn(Success(PointedGraph[repository.Rdf](factory.createBNode(""))))

      val brokenJson = Json.obj(
        "entryType" -> entryType.entryType,
        "id" -> entryType.id
      )
      val request = FakeRequest(
        PUT,
        s"/courses/$course/reportCardEntries/$entry/types/${entryType.id}",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> mimeType)),
        brokenJson
      )

      val result = controller.update(course.toString, entry.toString, entryType.id.toString)(request)

      status(result) shouldBe BAD_REQUEST
      contentType(result) shouldBe Some("application/json")
    }

    "not update a report card entry type when there is an exception" in {
      val entryType = types.head
      val toUpdate = ReportCardEntryType(entryType.entryType, !entryType.bool, entryType.int, entryType.id)
      val course = UUID.randomUUID()
      val errorMessage = "Oops, something went wrong"

      when(repository.update(anyObject())(anyObject(), anyObject())).thenReturn(Failure(new Throwable(errorMessage)))

      val request = FakeRequest(
        PUT,
        s"/courses/$course/reportCardEntries/$entry/types/${entryType.id}",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> mimeType)),
        Json.toJson(toUpdate)
      )

      val result = controller.update(course.toString, entry.toString, entryType.id.toString)(request)

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "errors" -> errorMessage
      )
    }

    "successfully return all report card entry types for a given appointment" in {
      import ReportCardEntryTypeController._
      import scala.util.Random.nextInt

      val student = UUID.randomUUID
      val labwork = UUID.randomUUID
      val all = ReportCardEntryType.all

      val entries = (0 until 5).map( n =>
        ReportCardEntry(student, labwork, n.toString, LocalDate.now.plusWeeks(n), LocalTime.now.plusHours(n), LocalTime.now.plusHours(n + 1), UUID.randomUUID, all.take(nextInt(all.size)))
      ).toSet
      val chosen = entries.toVector(nextInt(all.size))

      when(repository.prepareQuery(anyObject())).thenReturn(query)
      when(qe.parse(anyObject())).thenReturn(sparqlOps.parseSelect("SELECT * where {}"))
      when(qe.execute(anyObject())).thenReturn(Success(Map.empty[String, List[Value]]))
      when(repository.getMany[ReportCardEntry](anyObject())(anyObject())).thenReturn(Success(entries.filter(_ == chosen)))

      val request = FakeRequest(
        GET,
        s"/courses/$course/reportCardEntries/types?$studentAttribute=$student&$dateAttribute=${chosen.date}&$startAttribute=${chosen.start}"
      )

      val result = controller.all(course)(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(chosen.entryTypes)
    }

    "fail when there are not attributes" in {
      val request = FakeRequest(
        GET,
        s"/courses/$course/reportCardEntries/types"
      )

      val result = controller.all(course)(request)

      status(result) shouldBe BAD_REQUEST
    }
  }
}
