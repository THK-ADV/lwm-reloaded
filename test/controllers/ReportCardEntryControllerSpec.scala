package controllers

import java.util.UUID

import base.StreamHandler._
import base.{StreamHandler, TestBaseDefinition}
import models._
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
import play.api.libs.json.Json
import play.api.test.Helpers._
import play.api.test.{FakeHeaders, FakeRequest}
import services.{ReportCardService, RoleService, SessionHandlingService}
import store.bind.Bindings
import store.sparql.{QueryEngine, QueryExecutor, SelectClause}
import store.{Namespace, SesameRepository}
import utils.LwmMimeType

import scala.util.{Failure, Success, Try}

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
  val student = SesameStudent("systemId", "last", "first", "email", "regId", UUID.randomUUID)
  val labwork = SesameLabwork("label", "desc", UUID.randomUUID, UUID.randomUUID, UUID.randomUUID)
  val room = Room("label", "desc")
  val entries = (0 until 2).map(n =>
    ReportCardEntry(student.id, labwork.id, n.toString, LocalDate.now.plusWeeks(n), LocalTime.now.plusHours(n), LocalTime.now.plusHours(n + 1), room.id, ReportCardEntryType.all)
  ).toSet
  val atomizedEntries = entries.map { e =>
    val rescheduledAtom = RescheduledAtom(e.date, e.start, e.end, room)
    ReportCardEntryAtom(student, labwork, e.label, e.date, e.start, e.end, room, e.entryTypes, Some(rescheduledAtom), e.invalidated, e.id)
  }

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
      import models.ReportCardEntry.writes

      val rescheduledEntry = {
        val rescheduled = Rescheduled(entry.date.plusDays(3), entry.start.plusHours(1), entry.end.plusHours(1), UUID.randomUUID)
        ReportCardEntry(entry.student, entry.labwork, entry.label, entry.date, entry.start, entry.end, entry.room, entry.entryTypes, Some(rescheduled), entry.invalidated, entry.id)
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
      contentType(result) shouldBe Some(mimeType.value)
      contentAsJson(result) shouldBe Json.toJson(rescheduledEntry)
    }

    "not update when there is an inconsistency" in {
      val rescheduledEntry = {
        val rescheduled = Rescheduled(entry.date.plusDays(3), entry.start.plusHours(1), entry.end.plusHours(1), UUID.randomUUID)
        ReportCardEntry(entry.student, entry.labwork, entry.label, entry.date, entry.start, entry.end, entry.room, entry.entryTypes, Some(rescheduled), entry.invalidated, entry.id)
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
        ReportCardEntry(entry.student, entry.labwork, entry.label, entry.date, entry.start, entry.end, entry.room, entry.entryTypes, Some(rescheduled), entry.invalidated, entry.id)
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

    def whenQueryIsPrepared = {
      when(repository.prepareQuery(anyObject())).thenReturn(query)
      when(qe.parse(anyObject())).thenReturn(sparqlOps.parseSelect("SELECT * where {}"))
      when(qe.execute(anyObject())).thenReturn(Success(Map.empty[String, List[Value]]))
    }

    def whenFiltered(returnValue: Try[Set[ReportCardEntry]]) = {
      whenQueryIsPrepared
      when(repository.getMany[ReportCardEntry](anyObject())(anyObject())).thenReturn(returnValue)
    }

    "successfully return a student's report card entries" in {
      whenFiltered(Success(entries))

      val request = FakeRequest(
        GET,
        s"/reportCardEntries/student/${student.id}"
      )

      val result = controller.get(student.id.toString)(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some(mimeType.value)
      contentAsJson(result) shouldBe Json.toJson(entries)
    }

    "return an empty json when entries are not found" in {
      whenFiltered(Success(Set.empty[ReportCardEntry]))

      val request = FakeRequest(
        GET,
        s"/reportCardEntries/student/${student.id}"
      )

      val result = controller.get(student.id.toString)(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some(mimeType.value)
      contentAsJson(result) shouldBe Json.toJson(Set.empty[ReportCardEntry])
    }

    "not return a student's report card entries when there is an exception" in {
      val errorMsg = "Oops, something went wrong"

      whenFiltered(Failure(new Throwable(errorMsg)))

      val request = FakeRequest(
        GET,
        s"/reportCardEntries/student/${student.id}"
      )

      val result = controller.get(student.id.toString)(request)

      status(result) shouldBe SERVICE_UNAVAILABLE
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "message" -> errorMsg
      )
    }

    "successfully return a student's report card entries atomized" in {
      import models.ReportCardEntry.writesAtom

      whenQueryIsPrepared
      doReturn(Success(entries))
        .doReturn(Success(atomizedEntries))
        .when(repository)
        .getMany(anyObject())(anyObject())

      val request = FakeRequest(
        GET,
        s"/atomic/reportCardEntries/student/${student.id}"
      )

      val result = controller.getAtomic(student.id.toString)(request)
      val json = atomizedEntries map (a => Json.toJson(a))

      status(result) shouldBe OK
      contentType(result) shouldBe Some(mimeType.value)
      contentAsString(result) shouldBe json.mkString("")
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
        s"/courses/${UUID.randomUUID}/reportCardEntries/schedules/$schedule",
        FakeHeaders(Seq(CONTENT_TYPE -> mimeType)),
        Json.obj("" -> "")
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
        s"/courses/${UUID.randomUUID}/reportCardEntries/schedules/$schedule",
        FakeHeaders(Seq(CONTENT_TYPE -> mimeType)),
        Json.obj("" -> "")
      )

      val result = controller.create(UUID.randomUUID.toString, schedule.toString)(request)

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "errors" -> errorMessage
      )
    }

    "filter with query" in {
      val realRepo = SesameRepository(namespace)
      val bindings: Bindings[Sesame] = Bindings[Sesame](namespace)

      import bindings.{CourseDescriptor, LabworkDescriptor, ReportCardEntryDescriptor, ScheduleEntryDescriptor}
      val localController = new ReportCardEntryController(realRepo, sessionService, namespace, roleService, reportCardService) {
        override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
          case _ => NonSecureBlock
        }

        override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
          case _ => NonSecureBlock
        }
      }

      val course1 = SesameCourse("", "", "", UUID.randomUUID, 1)

      val student1 = UUID.randomUUID()
      val labwork1 = SesameLabwork("", "", UUID.randomUUID, course1.id, UUID.randomUUID)
      val date1 = LocalDate.now
      val (start1, end1) = (LocalTime.parse(LocalTime.now.toString("HH:mm")), LocalTime.parse(LocalTime.now.plusHours(2).toString("HH:mm")))
      val room1 = UUID.randomUUID()

      val student2 = UUID.randomUUID()
      val labwork2 = SesameLabwork("", "", UUID.randomUUID, course1.id, UUID.randomUUID)
      val date2 = LocalDate.now plusDays 2
      val (start2, end2) = (LocalTime.parse(LocalTime.now.plusHours(14).toString("HH:mm")), LocalTime.parse(LocalTime.now.plusHours(16).toString("HH:mm")))
      val room2 = UUID.randomUUID()

      val student3 = UUID.randomUUID()

      val scheduleEntry1 = ScheduleEntry(labwork1.id, start1, end1, date1, room1, Set(User.randomUUID), UUID.randomUUID())
      val scheduleEntry2 = ScheduleEntry(labwork1.id, start2, end2, date1, room2, Set(User.randomUUID), UUID.randomUUID())
      val entry1 = ReportCardEntry(student1, labwork1.id, "Label 1", date1, start1, end1, room1, Set(ReportCardEntryType.Certificate, ReportCardEntryType.Attendance))
      val entry2 = ReportCardEntry(student2, labwork2.id, "Label 2", date2, start1, end1, room1, Set(ReportCardEntryType.Bonus, ReportCardEntryType.Attendance))
      val entry3 = ReportCardEntry(student1, labwork1.id, "Label 3", date1, start2, end2, room2, Set(ReportCardEntryType.Certificate, ReportCardEntryType.Bonus))
      val entry4 = ReportCardEntry(student3, labwork1.id, "Label 4", date1, start1, end1, room1, Set(ReportCardEntryType.Supplement),
        Some(Rescheduled(date1, start2, end2, room2)))

      realRepo addMany List(course1)
      realRepo addMany List(labwork1, labwork2)
      realRepo addMany List(entry1, entry2, entry3, entry4)
      realRepo addMany List(scheduleEntry1, scheduleEntry2)

      val requestWithDate = FakeRequest(
        GET,
        s"/courses/${course1.id}/reportCardEntries?date=$date1"
      )

      val requestWithTime = FakeRequest(
        GET,
        s"/courses/${course1.id}/reportCardEntries?start=$start1"
      )

      val requestWithDateAndTimeAndRoom = FakeRequest(
        GET,
        s"/courses/${course1.id}/reportCardEntries?date=$date1&start=$start1&room=$room1"
      )

      val requestWithScheduleEntry = FakeRequest(
        GET,
        s"/courses/${course1.id}/scheduleEntries/${scheduleEntry1.id}/reportCardEntries"
      )

      val result1 = localController.all(course1.id.toString)(requestWithDate)
      val result2 = localController.all(course1.id.toString)(requestWithTime)
      val result3 = localController.all(course1.id.toString)(requestWithDateAndTimeAndRoom)
      val result4 = localController.allFromScheduleEntry(course1.id.toString, scheduleEntry1.id.toString)(requestWithScheduleEntry)
      val result5 = localController.allFromScheduleEntry(course1.id.toString, scheduleEntry2.id.toString)(requestWithScheduleEntry)
      val expected1 = Set(entry1, entry3, entry4)
      val expected2 = Set(entry1, entry2, entry4)
      val expected3 = Set(entry1, entry4)
      val expected4 = Set(entry4, entry1)
      val expected5 = Set(entry3, entry4)

      List(result1, result2, result3, result4, result5) zip List(expected1, expected2, expected3, expected4, expected5) foreach {
        case (result, expected) =>
          typedContentFromStream[ReportCardEntry](result) shouldBe expected
      }
    }

    "copy an existing reportcard from one student to another" in {
      import controllers.ReportCardCopyRequest.writes

      val destLab = UUID.randomUUID
      val destStudent = UUID.randomUUID
      val copy = ReportCardCopyRequest(labwork.id, student.id, destLab, destStudent)

      val request = FakeRequest(
        POST,
        s"/courses/$course/reportCardEntries/copy",
        FakeHeaders(Seq(CONTENT_TYPE -> mimeType)),
        Json.toJson(copy)
      )

      whenFiltered(Success(entries))
      when(repository.addMany[ReportCardEntry](anyObject())(anyObject())).thenReturn(Success(Set.empty[PointedGraph[Sesame]]))

      val result = controller.copy(course)(request)
      val resultEntries = typedContentFromStream[ReportCardEntry](result)

      status(result) shouldBe OK
      resultEntries.forall { entry =>
        entry.student == destStudent && entry.labwork == destLab
      } shouldBe true

      resultEntries.zip(entries).forall {
        case (l, r) => l.start.isEqual(r.start) &&
          l.end.isEqual(r.end) &&
          l.date.isEqual(r.date) &&
          l.room == r.room &&
          l.id != r.id &&
          l.rescheduled == r.rescheduled &&
          l.entryTypes.zip(r.entryTypes).forall {
            case ((lt, rt)) => lt.entryType == rt.entryType &&
              lt.bool == false &&
              lt.int == 0 &&
              lt.id != rt.id
          }
      } shouldBe true
    }

    "not copy an existing reportcard when precondition fails" in {
      import controllers.ReportCardCopyRequest.writes

      val copy = ReportCardCopyRequest(labwork.id, student.id, UUID.randomUUID, student.id)

      val request = FakeRequest(
        POST,
        s"/courses/$course/reportCardEntries/copy",
        FakeHeaders(Seq(CONTENT_TYPE -> mimeType)),
        Json.toJson(copy)
      )

      val result = controller.copy(course)(request)

      status(result) shouldBe PRECONDITION_FAILED
      contentFromStream(result).head shouldBe Json.obj(
        "status" -> "KO",
        "message" -> "srcStudent and destStudent should be different"
      )
    }
  }
}
