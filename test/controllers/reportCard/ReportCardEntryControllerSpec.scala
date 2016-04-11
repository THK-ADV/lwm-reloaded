package controllers.reportCard

import java.util.UUID

import base.TestBaseDefinition
import models.Room
import models.labwork._
import models.users.Student
import org.joda.time.{LocalDate, LocalTime}
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.openrdf.model.Value
import org.openrdf.model.impl.ValueFactoryImpl
import org.scalatest.WordSpec
import org.scalatest.mock.MockitoSugar.mock
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.{SesameModule, Sesame}
import play.api.http.HeaderNames
import play.api.libs.json.Json
import play.api.test.Helpers._
import play.api.test.{FakeHeaders, FakeRequest}
import services.{ReportCardService, RoleService, SessionHandlingService}
import store.sparql.{QueryEngine, SelectClause, QueryExecutor}
import store.{Namespace, SesameRepository}
import utils.LwmMimeType

import scala.util.{Failure, Success}

class ReportCardEntryControllerSpec extends WordSpec with TestBaseDefinition with SesameModule {

  val repository = mock[SesameRepository]
  val roleService = mock[RoleService]
  val reportCardService = mock[ReportCardService]
  val namespace = Namespace("http://lwm.gm.th-koeln.de")
  val sessionService = mock[SessionHandlingService]
  val factory = ValueFactoryImpl.getInstance()
  val qe = mock[QueryExecutor[SelectClause]]
  val query = QueryEngine.empty(qe)

  val mimeType = LwmMimeType.reportCardEntryV1Json
  val student = Student("systemId", "last", "first", "email", "regId", UUID.randomUUID)
  val labwork = Labwork("label", "desc", UUID.randomUUID, UUID.randomUUID, UUID.randomUUID)
  val room = Room("label", "desc")
  val entries = (0 until 2).map( n =>
    ReportCardEntry(student.id, labwork.id, n.toString, LocalDate.now.plusWeeks(n), LocalTime.now.plusHours(n), LocalTime.now.plusHours(n + 1), room.id, ReportCardEntryType.all)
  ).toSet
  val atomizedEntries = entries.map(e =>
    ReportCardEntryAtom(student, labwork, e.label, e.date, e.start, e.end, room, e.entryTypes, e.rescheduled, e.id)
  )

  val entry = entries.head
  val course = UUID.randomUUID.toString

  val controller: ReportCardEntryController = new ReportCardEntryController(repository, sessionService, namespace, roleService, reportCardService) {

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
        ReportCardEntry(entry.student, entry.labwork, entry.label, entry.date, entry.start, entry.end, entry.room, entry.entryTypes, Some(rescheduled), entry.id)
      }

      when(repository.update(anyObject())(anyObject(), anyObject())).thenReturn(Success(PointedGraph[repository.Rdf](factory.createBNode(""))))

      val request = FakeRequest(
        GET,
        s"/courses/$course/reportCardEntries/${entry.id}",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> mimeType)),
        Json.toJson(rescheduledEntry)
      )
      val result = controller.update(course, entry.id.toString)(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(rescheduledEntry)
    }

    "not update when there is an inconsistency" in {
      val rescheduledEntry = {
        val rescheduled = Rescheduled(entry.date.plusDays(3), entry.start.plusHours(1), entry.end.plusHours(1), UUID.randomUUID)
        ReportCardEntry(entry.student, entry.labwork, entry.label, entry.date, entry.start, entry.end, entry.room, entry.entryTypes, Some(rescheduled), entry.id)
      }

      val request = FakeRequest(
        GET,
        s"/courses/$course/reportCardEntries/${entry.id}",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> mimeType)),
        Json.toJson(rescheduledEntry)
      )
      val result = controller.update(course, UUID.randomUUID.toString)(request)

      status(result) shouldBe BAD_REQUEST
      contentType(result) shouldBe Some("application/json")
    }

    "not update a report card entry type with invalid json data" in {
      val invalidJson = Json.obj("first" -> 0)
      val request = FakeRequest(
        GET,
        s"/courses/$course/reportCardEntries/${entry.id}",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> mimeType)),
        invalidJson
      )
      val result = controller.update(course, entry.id.toString)(request)

      status(result) shouldBe BAD_REQUEST
      contentType(result) shouldBe Some("application/json")
    }

    "not update a report card entry type when there is an exception" in {
      val errorMessage = "Oops, something went wrong"
      val rescheduledEntry = {
        val rescheduled = Rescheduled(entry.date.plusDays(3), entry.start.plusHours(1), entry.end.plusHours(1), UUID.randomUUID)
        ReportCardEntry(entry.student, entry.labwork, entry.label, entry.date, entry.start, entry.end, entry.room, entry.entryTypes, Some(rescheduled), entry.id)
      }

      when(repository.update(anyObject())(anyObject(), anyObject())).thenReturn(Failure(new Throwable(errorMessage)))

      val request = FakeRequest(
        GET,
        s"/courses/$course/reportCardEntries/${entry.id}",
        FakeHeaders(Seq(HeaderNames.CONTENT_TYPE -> mimeType)),
        Json.toJson(rescheduledEntry)
      )
      val result = controller.update(course, entry.id.toString)(request)

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "errors" -> errorMessage
      )
    }

    "successfully return a student's report card entries" in {
      when(repository.prepareQuery(anyObject())).thenReturn(query)
      when(qe.parse(anyObject())).thenReturn(sparqlOps.parseSelect("SELECT * where {}"))
      when(qe.execute(anyObject())).thenReturn(Success(Map.empty[String, List[Value]]))
      when(repository.getMany[ReportCardEntry](anyObject())(anyObject())).thenReturn(Success(entries))

      val request = FakeRequest(
        GET,
        s"/reportCardEntries/student/${student.id}"
      )

      val result = controller.get(student.id.toString)(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(entries)
    }

    "return an empty json when entries are not found" in {
      when(repository.prepareQuery(anyObject())).thenReturn(query)
      when(qe.parse(anyObject())).thenReturn(sparqlOps.parseSelect("SELECT * where {}"))
      when(qe.execute(anyObject())).thenReturn(Success(Map.empty[String, List[Value]]))
      when(repository.getMany[ReportCardEntry](anyObject())(anyObject())).thenReturn(Success(Set.empty[ReportCardEntry]))

      val request = FakeRequest(
        GET,
        s"/reportCardEntries/student/${student.id}"
      )

      val result = controller.get(student.id.toString)(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(Set.empty[ReportCardEntry])
    }

    "not return a student's report card entries when there is an exception" in {
      val errorMsg = "Oops, something went wrong"

      when(repository.prepareQuery(anyObject())).thenReturn(query)
      when(qe.parse(anyObject())).thenReturn(sparqlOps.parseSelect("SELECT * where {}"))
      when(qe.execute(anyObject())).thenReturn(Success(Map.empty[String, List[Value]]))
      when(repository.getMany[ReportCardEntry](anyObject())(anyObject())).thenReturn(Failure(new Throwable(errorMsg)))

      val request = FakeRequest(
        GET,
        s"/reportCardEntries/student/${student.id}"
      )

      val result = controller.get(student.id.toString)(request)

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "errors" -> errorMsg
      )
    }

    "successfully return a student's report card entries atomized" in {
      import ReportCardEntry.atomicWrites

      when(repository.prepareQuery(anyObject())).thenReturn(query)
      when(qe.parse(anyObject())).thenReturn(sparqlOps.parseSelect("SELECT * where {}"))
      when(qe.execute(anyObject())).thenReturn(Success(Map.empty[String, List[Value]]))
      when(repository.getMany[ReportCardEntry](anyObject())(anyObject())).thenReturn(Success(entries))

        doReturn(Success(Some(student))).
          doReturn(Success(Some(labwork))).
          doReturn(Success(Some(room))).
          doReturn(Success(Some(student))).
          doReturn(Success(Some(labwork))).
          doReturn(Success(Some(room))).
          when(repository).get(anyObject())(anyObject())

      val request = FakeRequest(
        GET,
        s"/atomic/reportCardEntries/student/${student.id}"
      )

      val result = controller.getAtomic(student.id.toString)(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(atomizedEntries)
    }

    "not return a student's report card entries atomized when there is an exception" in {
      val errorMessage = "Oops, something went wrong"

      when(repository.prepareQuery(anyObject())).thenReturn(query)
      when(qe.parse(anyObject())).thenReturn(sparqlOps.parseSelect("SELECT * where {}"))
      when(qe.execute(anyObject())).thenReturn(Success(Map.empty[String, List[Value]]))
      when(repository.getMany[ReportCardEntry](anyObject())(anyObject())).thenReturn(Success(entries))

      doReturn(Failure(new Throwable(errorMessage))).
        doReturn(Success(Some(labwork))).
        doReturn(Success(Some(room))).
        when(repository).get(anyObject())(anyObject())

      val request = FakeRequest(
        GET,
        s"/atomic/reportCardEntries/student/${student.id}"
      )

      val result = controller.getAtomic(student.id.toString)(request)

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "errors" -> errorMessage
      )
    }

    "successfully return report card entries for a given attributes" in {
      import ReportCardEntryController._

      when(repository.prepareQuery(anyObject())).thenReturn(query)
      when(qe.parse(anyObject())).thenReturn(sparqlOps.parseSelect("SELECT * where {}"))
      when(qe.execute(anyObject())).thenReturn(Success(Map.empty[String, List[Value]]))
      when(repository.getMany[ReportCardEntry](anyObject())(anyObject())).thenReturn(Success(entries))

      val request = FakeRequest(
        GET,
        s"/reportCardEntries?$studentAttribute=${student.id}&$labworkAttribute=${labwork.id}"
      )

      val result = controller.all(course)(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(entries)
    }

    "fail when there are not attributes" in {
      val request = FakeRequest(
        GET,
        s"/reportCardEntries/"
      )

      val result = controller.all(course)(request)

      status(result) shouldBe BAD_REQUEST
      contentType(result) shouldBe Some("application/json")
    }

    "successfully create report cards for given schedule" in {
      val schedule = UUID.randomUUID

      when(repository.prepareQuery(anyObject())).thenReturn(query)
      when(qe.execute(anyObject())).thenReturn(Success(
        Map("plan" -> List(factory.createURI(AssignmentPlan.generateUri(UUID.randomUUID())(namespace))))
      ))
      doReturn(Success(Some(AssignmentPlan.empty))).
        doReturn(Success(Some(Schedule.empty))).
        doReturn(Success(Some(Group.empty))).
        when(repository).get(anyObject())(anyObject())
      when(reportCardService.reportCards(anyObject(), anyObject())).thenReturn(Set.empty[ReportCardEntry])
      when(repository.addMany(anyObject())(anyObject())).thenReturn(Success(Set.empty[PointedGraph[Sesame]]))

      val request = FakeRequest(
        POST,
        s"/courses/${UUID.randomUUID}/reportCardEntries/schedules/$schedule"
      )

      val result = controller.create(UUID.randomUUID.toString, schedule.toString)(request)

      status(result) shouldBe CREATED
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "OK",
        "message" -> s"Created report card entries for schedule $schedule"
      )
    }

    "fail publishing a schedule when there is a exception" in {
      val schedule = UUID.randomUUID
      val errorMessage = "Oops, something went wrong"

      when(repository.prepareQuery(anyObject())).thenReturn(query)
      when(qe.execute(anyObject())).thenReturn(Success(
        Map("plan" -> List(factory.createURI(AssignmentPlan.generateUri(UUID.randomUUID())(namespace))))
      ))
      doReturn(Failure(new Exception(errorMessage))).
        doReturn(Success(Some(Schedule.empty))).
        doReturn(Success(Some(Group.empty))).
        when(repository).get(anyObject())(anyObject())
      when(reportCardService.reportCards(anyObject(), anyObject())).thenReturn(Set.empty[ReportCardEntry])
      when(repository.addMany(anyObject())(anyObject())).thenReturn(Success(Set.empty[PointedGraph[Sesame]]))

      val request = FakeRequest(
        POST,
        s"/courses/${UUID.randomUUID}/reportCardEntries/schedules/$schedule"
      )

      val result = controller.create(UUID.randomUUID.toString, schedule.toString)(request)

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "errors" -> errorMessage
      )
    }
  }
}
