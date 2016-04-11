package controllers.reportCard

import java.util.UUID

import base.TestBaseDefinition
import models.labwork.{ReportCardEntry, ReportCardEntryType}
import org.joda.time.{LocalDate, LocalTime}
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.openrdf.model.Value
import org.openrdf.model.impl.ValueFactoryImpl
import org.scalatest.WordSpec
import org.scalatest.mock.MockitoSugar.mock
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.{Sesame, SesameModule}
import play.api.http.HeaderNames
import play.api.libs.json.{JsArray, Json}
import play.api.test.Helpers._
import play.api.test.{FakeHeaders, FakeRequest}
import services.{RoleService, SessionHandlingService}
import store.Prefixes.LWMPrefix
import store.bind.Bindings
import store.sparql.{QueryEngine, QueryExecutor, SelectClause}
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
        s"/courses/$course/reportCardEntryTypes/${entryType.id}",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> mimeType)),
        Json.toJson(toUpdate)
      )

      val result = controller.update(course, entryType.id.toString)(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(toUpdate)
    }

    "not update when there is an inconsistency" in {
      val entryType = types.head
      val toUpdate = ReportCardEntryType(entryType.entryType, !entryType.bool, entryType.int, entryType.id)

      val request = FakeRequest(
        PUT,
        s"/courses/$course/reportCardEntryTypes/${entryType.id}",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> mimeType)),
        Json.toJson(toUpdate)
      )

      val result = controller.update(course, UUID.randomUUID.toString)(request)

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
        s"/courses/$course/reportCardEntryTypes/${entryType.id}",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> mimeType)),
        brokenJson
      )

      val result = controller.update(course.toString, entryType.id.toString)(request)

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
        s"/courses/$course/reportCardEntryTypes/${entryType.id}",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> mimeType)),
        Json.toJson(toUpdate)
      )

      val result = controller.update(course.toString, entryType.id.toString)(request)

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
        s"/courses/$course/reportCardEntryTypes?$studentAttribute=$student&$dateAttribute=${chosen.date}&$startAttribute=${chosen.start}"
      )

      val result = controller.all(course)(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(chosen.entryTypes)
    }

    "fail when there are no attributes" in {
      val request = FakeRequest(
        GET,
        s"/courses/$course/reportCardEntryTypes"
      )

      val result = controller.all(course)(request)

      status(result) shouldBe BAD_REQUEST
    }

    "filter with query" in {
      val realRepo = SesameRepository(namespace)
      val lwm = LWMPrefix[realRepo.Rdf](realRepo.rdfOps, realRepo.rdfOps)
      val bindings: Bindings[Sesame] = Bindings[Sesame](namespace)

      import bindings.ReportCardEntryBinding._

      val localController = new ReportCardEntryTypeController(realRepo, sessionService, namespace, roleService) {
        override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
          case _ => NonSecureBlock
        }

        override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
          case _ => NonSecureBlock
        }
      }

      val course = UUID.randomUUID()

      val student1 = UUID.randomUUID()
      val labwork1 = UUID.randomUUID()
      val date1 = LocalDate.now
      val (start1, end1) = (LocalTime.parse(LocalTime.now.toString("HH:mm")), LocalTime.parse(LocalTime.now.plusHours(2).toString("HH:mm")))
      val room1 = UUID.randomUUID()

      val student2 = UUID.randomUUID()
      val labwork2 = UUID.randomUUID()
      val date2 = LocalDate.now plusDays 2
      val (start2, end2) = (LocalTime.parse(LocalTime.now.plusHours(14).toString("HH:mm")), LocalTime.parse(LocalTime.now.plusHours(16).toString("HH:mm")))
      val room2 = UUID.randomUUID()


      val entry1 = ReportCardEntry(student1, labwork1, "Label 1", date1, start1, end1, room1, Set(ReportCardEntryType.Certificate, ReportCardEntryType.Attendance))
      val entry2 = ReportCardEntry(student2, labwork2, "Label 2", date2, start1, end1, room1, Set(ReportCardEntryType.Bonus, ReportCardEntryType.Attendance))
      val entry3 = ReportCardEntry(student1, labwork1, "Label 3", date1, start2, end2, room2, Set(ReportCardEntryType.Certificate, ReportCardEntryType.Bonus))

      realRepo.addMany[ReportCardEntry](List(entry1, entry2, entry3))

      val requestWithDate = FakeRequest(
        GET,
        s"/courses/$course/reportCardEntryTypes?date=$date1"
      )

      val requestWithTime = FakeRequest(
        GET,
        s"/courses/$course/reportCardEntryTypes?start=$start1"
      )

      val result1 = localController.all(course.toString)(requestWithDate)
      val result2 = localController.all(course.toString)(requestWithTime)
      val expected1 = entry1.entryTypes ++ entry3.entryTypes
      val expected2 = entry1.entryTypes ++ entry2.entryTypes

      contentAsJson(result1).asInstanceOf[JsArray].value foreach { entry =>
      expected1 contains Json.fromJson[ReportCardEntryType](entry).get shouldBe true
      }

      contentAsJson(result2).asInstanceOf[JsArray].value foreach { entry =>
        expected2 contains Json.fromJson[ReportCardEntryType](entry).get shouldBe true
      }
    }
  }
}
