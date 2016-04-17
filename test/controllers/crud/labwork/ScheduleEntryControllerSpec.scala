package controllers.crud.labwork

import org.joda.time.{LocalDate, LocalTime}
import java.util.UUID

import base.TestBaseDefinition
import models.Room
import models.labwork._
import models.users.Employee
import org.scalatest.WordSpec
import org.mockito.Matchers._
import org.scalatest.mock.MockitoSugar.mock
import org.mockito.Mockito._
import org.openrdf.model.impl.ValueFactoryImpl
import org.w3.banana.PointedGraph
import play.api.libs.json.Json
import play.api.test.{FakeHeaders, FakeRequest}
import play.api.test.Helpers._
import services.{RoleService, SessionHandlingService}
import store.bind.Bindings
import store.sparql.{QueryEngine, QueryExecutor, SelectClause}
import store.{Namespace, SesameRepository}
import utils.LwmMimeType

import scala.util.Success

class ScheduleEntryControllerSpec extends WordSpec with TestBaseDefinition {

  val repository = mock[SesameRepository]
  val sessionService = mock[SessionHandlingService]
  val roleService = mock[RoleService]
  val ns = Namespace("test://lwm.gm.fh-koeln.de")
  val factory = ValueFactoryImpl.getInstance()
  val qe = mock[QueryExecutor[SelectClause]]
  val query = QueryEngine.empty(qe)

  val mimeType = Some(LwmMimeType.scheduleEntryV1Json.value)

  val controller: ScheduleEntryController = new ScheduleEntryController(repository, sessionService, ns, roleService) {
    override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }
    override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }
  }

  def entries(labwork: UUID): Int => Vector[ScheduleEntry] = amount => ((0 until amount) map { _ =>
    ScheduleEntry(labwork, LocalTime.now, LocalTime.now plusHours 2, LocalDate.now, UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID())
  }).toVector

  //when(sessionService.isValid(anyObject())).thenReturn(Future.successful(true))

  "A ScheduleEntry controller" should {


    "get specific entries" in {
      val entry = entries(UUID.randomUUID())(1).head
      when(repository.get[ScheduleEntry](anyObject())(anyObject())).thenReturn(Success(Some(entry)))

      val labworkID = UUID.randomUUID()
      val courseID = UUID.randomUUID()
      val entryID = UUID.randomUUID()

      val request = FakeRequest(
        GET,
        s"/courses/$courseID/labwork/$labworkID/entries/$entryID"
      )

      val result = controller.get(courseID.toString, entryID.toString)(request)

      status(result) shouldBe OK
      contentType(result) shouldBe mimeType
      contentAsJson(result) shouldBe Json.toJson(entry)
    }

    "get specific entries atomised" in {
      import ScheduleEntry.format
      val labworkID = UUID.randomUUID()
      val courseID = UUID.randomUUID()

      val entry = entries(labworkID)(1).head
      val group = Group("Label", labworkID, Set.empty)
      val supervisor = Employee("systemid", "lastname", "firstname", "email", "status")
      val room = Room("label", "description")

      doReturn(Success(Some(entry))).
        doReturn(Success(Some(group))).
        doReturn(Success(Some(supervisor))).
        doReturn(Success(Some(room))).
        when(repository).get(anyObject())(anyObject())

      val scheduleEntry = ScheduleEntryAtom(entry.start, entry.end, entry.date, room, supervisor, group)

      val request = FakeRequest(
        GET,
        s"/courses/$courseID/labwork/$labworkID/atomic/entries/${entry.id}"
      )

      val result = controller.getAtomic(courseID.toString, entry.id.toString)(request)

      status(result) shouldBe OK
      contentType(result) shouldBe mimeType
      contentAsJson(result) shouldBe Json.toJson(scheduleEntry)
    }

    "get all entries" in {
      val labworkID = UUID.randomUUID()
      val courseID = UUID.randomUUID()

      val entry = entries(labworkID)(3)
      val group1 = Group("Label1", labworkID, Set.empty)
      val group2 = Group("Label2", labworkID, Set.empty)
      val supervisor1 = Employee("systemid1", "lastname1", "firstname1", "email1", "status1")
      val supervisor2 = Employee("systemid2", "lastname2", "firstname2", "email2", "status2")
      val room1 = Room("label1", "description1")
      val room2 = Room("label1", "description2")


      when(repository.get[ScheduleEntry](anyObject(), anyObject())).thenReturn(Success(Set(entry(0), entry(1), entry(2))))
      val request = FakeRequest(
        GET,
        s"/courses/$courseID/labwork/$labworkID/entries"
      )

      val result = controller.all(courseID.toString)(request)

      val jsVals = entry map (e => Json.toJson(e))

      status(result) shouldBe OK
      contentType(result) shouldBe mimeType
      contentAsString(result) shouldBe jsVals.foldLeft("")(_ + _)
    }

    "get all entries atomised" in {
      import ScheduleEntry.format

      val labworkID = UUID.randomUUID()
      val courseID = UUID.randomUUID()

      val entry = entries(labworkID)(3)
      val group1 = Group("Label1", labworkID, Set.empty)
      val group2 = Group("Label2", labworkID, Set.empty)
      val supervisor1 = Employee("systemid1", "lastname1", "firstname1", "email1", "status1")
      val supervisor2 = Employee("systemid2", "lastname2", "firstname2", "email2", "status2")
      val room1 = Room("label1", "description1")
      val room2 = Room("label1", "description2")

      when(repository.get[ScheduleEntry](anyObject(), anyObject())).thenReturn(Success(Set(entry(0), entry(1), entry(2))))

      doReturn(Success(Some(group1))).
        doReturn(Success(Some(supervisor1))).
        doReturn(Success(Some(room1))).
        doReturn(Success(Some(group2))).
        doReturn(Success(Some(supervisor1))).
        doReturn(Success(Some(room1))).
        doReturn(Success(Some(group2))).
        doReturn(Success(Some(supervisor2))).
        doReturn(Success(Some(room2))).
        when(repository).get(anyObject())(anyObject())

      val scheduleEntry1 = ScheduleEntryAtom(entry(0).start, entry(0).end, entry(0).date, room1, supervisor1, group1)
      val scheduleEntry2 = ScheduleEntryAtom(entry(1).start, entry(1).end, entry(1).date, room1, supervisor1, group2)
      val scheduleEntry3 = ScheduleEntryAtom(entry(2).start, entry(2).end, entry(2).date, room2, supervisor2, group2)

      val request = FakeRequest(
        GET,
        s"/courses/$courseID/labwork/$labworkID/atomic/entries"
      )

      val result = controller.allAtomic(courseID.toString)(request)

      val jsVals = List(Json.toJson(scheduleEntry1), Json.toJson(scheduleEntry2), Json.toJson(scheduleEntry3))

      status(result) shouldBe OK
      contentType(result) shouldBe mimeType
      contentAsString(result) shouldBe jsVals.foldLeft("")(_ + _)
    }


    "update entries and respond with updated version" in {
      val entry = entries(UUID.randomUUID())(1).head

      val labworkID = UUID.randomUUID()
      val courseID = UUID.randomUUID()

      when(repository.update(anyObject())(anyObject(), anyObject())).thenReturn(Success(PointedGraph[repository.Rdf](factory.createBNode("#"))))

      val request = FakeRequest(
        PUT,
        s"/courses/$courseID/labwork/$labworkID/entries/${entry.id}",
        FakeHeaders(Seq("Content-Type" -> mimeType.get)),
        Json.toJson(entry)
      )

      val result = controller.update(courseID.toString, entry.id.toString)(request)

      status(result) shouldBe OK
      contentType(result) shouldBe mimeType
      contentAsJson(result) shouldBe Json.toJson(entry)
    }


    "update entries and respond with the atomised atomic version" in {
      import ScheduleEntry.format
      val entry = entries(UUID.randomUUID())(1).head

      val labworkID = UUID.randomUUID()
      val courseID = UUID.randomUUID()

      when(repository.update(anyObject())(anyObject(), anyObject())).thenReturn(Success(PointedGraph[repository.Rdf](factory.createBNode("#"))))

      val group = Group("Label1", labworkID, Set.empty)
      val supervisor = Employee("systemid1", "lastname1", "firstname1", "email1", "status1")
      val room = Room("label1", "description1")

      doReturn(Success(Some(group))).
        doReturn(Success(Some(supervisor))).
        doReturn(Success(Some(room))).
        when(repository).get(anyObject())(anyObject())

      val scheduleEntry = ScheduleEntryAtom(entry.start, entry.end, entry.date, room, supervisor, group)

      val request = FakeRequest(
        PUT,
        s"/courses/$courseID/labwork/$labworkID/entries/${entry.id}",
        FakeHeaders(Seq("Content-Type" -> mimeType.get)),
        Json.toJson(entry)
      )

      val result = controller.updateAtomic(courseID.toString, entry.id.toString)(request)

      status(result) shouldBe OK
      contentType(result) shouldBe mimeType
      contentAsJson(result) shouldBe Json.toJson(scheduleEntry)
    }


    "filter entries by various attributes" in {
      val realRepository = SesameRepository(ns)
      val bindings = Bindings[realRepository.Rdf](ns)
      val realController: ScheduleEntryController = new ScheduleEntryController(realRepository, sessionService, ns, roleService) {
        override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
          case _ => NonSecureBlock
        }

        override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
          case _ => NonSecureBlock
        }
      }

      import bindings.GroupBinding._
      import bindings.RoomBinding._
      import bindings.UserBinding._
      import bindings.ScheduleEntryBinding._

      val courseID = UUID.randomUUID()

      val labwork1 = UUID.randomUUID()
      val labwork2 = UUID.randomUUID()

      val group1 = Group("Label1", labwork1, Set.empty)
      val group2 = Group("Label2", labwork2, Set.empty)
      val supervisor1 = Employee("systemid1", "lastname1", "firstname1", "email1", "status1")
      val supervisor2 = Employee("systemid2", "lastname2", "firstname2", "email2", "status2")
      val room1 = Room("label1", "description1")
      val room2 = Room("label1", "description2")
      val start1 = LocalTime.now
      val start2 = LocalTime.now plusHours 2
      val start3 = LocalTime.now plusHours 4

      val end1 = LocalTime.now plusHours 2
      val end2 = LocalTime.now plusHours 4
      val end3 = LocalTime.now plusHours 6

      val date1 = LocalDate.now
      val date2 = LocalDate.now plusDays 1
      val date3 = LocalDate.now plusDays 2
      val date4 = LocalDate.now plusDays 4

      val sentry1 = ScheduleEntry(labwork1, start1, end1, date1, room1.id, supervisor1.id, group1.id)
      val sentry2 = ScheduleEntry(labwork1, start1, end2, date2, room1.id, supervisor1.id, group1.id)
      val sentry3 = ScheduleEntry(labwork2, start3, end3, date2, room2.id, supervisor2.id, group2.id)
      val sentry4 = ScheduleEntry(labwork2, start2, end2, date3, room2.id, supervisor2.id, group1.id)
      val sentry5 = ScheduleEntry(labwork1, start2, end2, date4, room1.id, supervisor1.id, group2.id)


      realRepository addMany List(group1, group2)
      realRepository addMany List(supervisor1, supervisor2)
      realRepository addMany List(room1, room2)
      realRepository addMany List(sentry1, sentry2, sentry3, sentry4, sentry5)

      val requestForLabwork = FakeRequest(
        GET,
        s"/courses/$courseID/labwork/$labwork1/entries?labwork=$labwork2"
      )

      val requestForGroup = FakeRequest(
        GET,
        s"/courses/$courseID/labwork/$labwork1/entries?group=${group1.id}"
      )

      val requestForSupervisor = FakeRequest(
        GET,
        s"/courses/$courseID/labwork/$labwork1/entries?supervisor=${supervisor1.id}"
      )

      val requestGroupSupervisor = FakeRequest(
        GET,
        s"/courses/$courseID/labwork/$labwork1/entries?group=${group2.id}&supervisor=${supervisor1.id}"
      )

      val requestForDate = FakeRequest(
        GET,
        s"/courses/$courseID/labwork/$labwork1/entries?date=$date2"
      )

      val requestForDateAndTime = FakeRequest(
        GET,
        s"/courses/$courseID/labwork/$labwork1/entries?date=$date2&start=$start3"
      )

      val requestForMinMax = FakeRequest(
        GET,
        s"/courses/$courseID/labwork/$labwork1/entries?dateRange=$date2,$date3"
      )

      val requestForGroupWithinDateRange = FakeRequest(
        GET,
        s"/courses/$courseID/labwork/$labwork1/entries?group=${group1.id}&dateRange=$date2,$date4"
      )

      val requestForGroupWithinDateRangeAndSup = FakeRequest(
        GET,
        s"/courses/$courseID/labwork/$labwork1/entries?group=${group1.id}&dateRange=$date2,$date4&supervisor=${supervisor2.id}"
      )

      val resultForLabwork = realController.all(courseID.toString)(requestForLabwork)
      val resultForGroup = realController.all(courseID.toString)(requestForGroup)
      val resultForSupervisor = realController.all(courseID.toString)(requestForSupervisor)
      val resultForGroupSupervisor = realController.all(courseID.toString)(requestGroupSupervisor)
      val resultForDate = realController.all(courseID.toString)(requestForDate)
      val resultForDateAndTime = realController.all(courseID.toString)(requestForDateAndTime)
      val resultForMinMax = realController.all(courseID.toString)(requestForMinMax)
      val resultForGroupWithinDateRange = realController.all(courseID.toString)(requestForGroupWithinDateRange)
      val resultForGroupWithinDateRangeAndSup = realController.all(courseID.toString)(requestForGroupWithinDateRangeAndSup)

      val valsForLabwork = List(Json.toJson(sentry4), Json.toJson(sentry3))
      val valsForGroup = List(Json.toJson(sentry4), Json.toJson(sentry2), Json.toJson(sentry1))
      val valsForSupervisor = List(Json.toJson(sentry5), Json.toJson(sentry2), Json.toJson(sentry1))
      val valsForGroupSupervisor = List(Json.toJson(sentry5))
      val valsForDate = List(Json.toJson(sentry3), Json.toJson(sentry2))
      val valsForDateAndTime = List(Json.toJson(sentry3))
      val valsForMinMax = List(Json.toJson(sentry4), Json.toJson(sentry3), Json.toJson(sentry2))
      val valsForGroupWithinDateRange = List(Json.toJson(sentry4), Json.toJson(sentry2))
      val valsForGroupWithinDateRangeAndSup = List(Json.toJson(sentry4))

      status(resultForLabwork) shouldBe OK
      status(resultForGroup) shouldBe OK
      status(resultForSupervisor) shouldBe OK
      status(resultForGroupSupervisor) shouldBe OK
      status(resultForDate) shouldBe OK
      status(resultForDateAndTime) shouldBe OK
      status(resultForMinMax) shouldBe OK
      status(resultForGroupWithinDateRange) shouldBe OK
      status(resultForGroupWithinDateRangeAndSup) shouldBe OK

      contentAsString(resultForLabwork) shouldBe valsForLabwork.foldLeft("")(_ + _)
      contentAsString(resultForGroup) shouldBe valsForGroup.foldLeft("")(_ + _)
      contentAsString(resultForSupervisor) shouldBe valsForSupervisor.foldLeft("")(_ + _)
      contentAsString(resultForGroupSupervisor) shouldBe valsForGroupSupervisor.foldLeft("")(_ + _)
      contentAsString(resultForDate) shouldBe valsForDate.foldLeft("")(_ + _)
      contentAsString(resultForDateAndTime) shouldBe valsForDateAndTime.foldLeft("")(_ + _)
      contentAsString(resultForMinMax) shouldBe valsForMinMax.foldLeft("")(_ + _)
      contentAsString(resultForGroupWithinDateRange) shouldBe valsForGroupWithinDateRange.foldLeft("")(_ + _)
      contentAsString(resultForGroupWithinDateRangeAndSup) shouldBe valsForGroupWithinDateRangeAndSup.foldLeft("")(_ + _)
    }
  }
}
