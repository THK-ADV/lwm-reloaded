package controllers.schedule

import java.util.UUID

import base.TestBaseDefinition
import models.{CourseAtom, Degree, Room}
import models.labwork._
import models.security.Permissions.labwork
import models.semester.Semester
import models.users.Employee
import org.joda.time.{LocalDate, LocalTime}
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.openrdf.model.impl.ValueFactoryImpl
import org.scalatest.WordSpec
import org.scalatest.mock.MockitoSugar.mock
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.SesameModule
import play.api.libs.json.Json
import play.api.mvc.{Action, AnyContent}
import play.api.test.Helpers._
import play.api.test.{FakeHeaders, FakeRequest}
import services.{RoleService, SessionHandlingService}
import store.bind.Bindings
import store.sparql.{QueryEngine, QueryExecutor, SelectClause}
import store.{Namespace, SesameRepository}
import utils.LwmMimeType

import scala.util.Success

class ScheduleEntryControllerSpec extends WordSpec with TestBaseDefinition with SesameModule {

  val repository = mock[SesameRepository]
  val sessionService = mock[SessionHandlingService]
  val roleService = mock[RoleService]
  val ns = Namespace("test://lwm.gm.fh-koeln.de")
  val factory = ValueFactoryImpl.getInstance()
  val qe = mock[QueryExecutor[SelectClause]]
  val query = QueryEngine.empty(qe)

  val mimeType = Some(LwmMimeType.scheduleEntryV1Json.value)

  val controller: ScheduleEntryController = new ScheduleEntryController(repository, sessionService, ns, roleService) {

    override def allFrom(course: String): Action[AnyContent] = Action { implicit request =>
      retrieveAll[ScheduleEntry]
        .map(set => chunk(set))
        .mapResult(enum => Ok.stream(enum).as(mimeType))
    }

    override def allAtomicFrom(course: String): Action[AnyContent] = Action { implicit request =>
      retrieveAll[ScheduleEntryAtom]
        .map(set => chunk(set))
        .mapResult(enum => Ok.stream(enum).as(mimeType))
    }

    override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }

    override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }
  }

  def entries(labwork: UUID): Int => Vector[ScheduleEntry] = amount => ((0 until amount) map { _ =>
    ScheduleEntry(labwork, LocalTime.now, LocalTime.now plusHours 2, LocalDate.now, UUID.randomUUID(), Set(UUID.randomUUID()), UUID.randomUUID())
  }).toVector

  def atomizeEntries(ents: Vector[ScheduleEntry]): Vector[ScheduleEntryAtom] = ents map { e =>
    val semester = Semester("label to pass", "abbrev to pass", LocalDate.now, LocalDate.now, LocalDate.now)
    val employee = Employee("systemId to pass", "last name to pass", "first name to pass", "email to pass", "employee")
    val courseAtom = CourseAtom("label to pass", "desc to pass", "abbrev to pass", employee, 1, None, UUID.randomUUID)
    val degree = Degree("label to pass", "abbrev to pass")

    val labworkAtom = LabworkAtom("labwork", "desc", semester, courseAtom, degree, false, published = false, None, e.labwork)
    val room = Room("room", "desc", None, e.room)
    val supervisor = e.supervisor map (Employee("systemid", "lastname", "firstname", "email", "supervisor", None, _))
    val group = Group("label", labworkAtom.id, Set(), None, e.group)
    ScheduleEntryAtom(labworkAtom, e.start, e.end, e.date, room, supervisor, group, None, e.id)
  }

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
      val labwork = Labwork("", "", UUID.randomUUID, UUID.randomUUID, UUID.randomUUID)
      val courseID = UUID.randomUUID()

      val entry = entries(labwork.id)(1)
      val atomized = atomizeEntries(entry)

      when(repository.get[ScheduleEntryAtom](anyObject())(anyObject())).thenReturn(Success(Some(atomized.head)))

      val request = FakeRequest(
        GET,
        s"/courses/$courseID/labwork/${labwork.id}/atomic/entries/${entry.head.id}"
      )

      val expected = Json.toJson(atomized.head)
      val result = controller.getAtomic(courseID.toString, entry.head.id.toString)(request)

      status(result) shouldBe OK
      contentType(result) shouldBe mimeType
      contentAsJson(result) shouldBe Json.toJson(expected)
    }

    "get all entries" in {
      val labworkID = UUID.randomUUID()
      val courseID = UUID.randomUUID()

      val entry = entries(labworkID)(3)

      when(repository.getAll[ScheduleEntry](anyObject())).thenReturn(Success(Set(entry(0), entry(1), entry(2))))
      val request = FakeRequest(
        GET,
        s"/courses/$courseID/labwork/$labworkID/entries"
      )

      val result = controller.allFrom(courseID.toString)(request)

      val jsVals = entry map (e => Json.toJson(e))

      status(result) shouldBe OK
      contentType(result) shouldBe mimeType
      contentAsString(result) shouldBe jsVals.mkString("")
    }

    "get all entries atomised" in {
      import ScheduleEntry.format

      val labwork = Labwork("", "", UUID.randomUUID, UUID.randomUUID, UUID.randomUUID)
      val courseID = UUID.randomUUID()

      val entry = entries(labwork.id)(3)
      val atomized = atomizeEntries(entry)

      when(repository.getAll[ScheduleEntryAtom](anyObject())).thenReturn(Success(atomized.toSet))

      val request = FakeRequest(
        GET,
        s"/courses/$courseID/labwork/${labwork.id}/atomic/entries"
      )

      val expected = atomized map (a => Json.toJson(a))
      val result = controller.allAtomicFrom(courseID.toString)(request)

      status(result) shouldBe OK
      contentType(result) shouldBe mimeType
      contentAsString(result) shouldBe expected.mkString("")
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
      val labwork = Labwork("", "", UUID.randomUUID, UUID.randomUUID, UUID.randomUUID)
      val courseID = UUID.randomUUID()

      val entry = entries(labwork.id)(1)
      val atomized = atomizeEntries(entry)

      when(repository.update(anyObject())(anyObject(), anyObject())).thenReturn(Success(PointedGraph[repository.Rdf](factory.createBNode("#"))))

      doReturn(Success(Some(atomized.head))).
        when(repository).get(anyObject())(anyObject())

      val request = FakeRequest(
        PUT,
        s"/courses/$courseID/atomic/scheduleEntries/${entry.head.id}",
        FakeHeaders(Seq("Content-Type" -> mimeType.get)),
        Json.toJson(entry.head)
      )

      val expected = Json.toJson(atomized.head)
      val result = controller.updateAtomic(courseID.toString, entry.head.id.toString)(request)

      status(result) shouldBe OK
      contentType(result) shouldBe mimeType
      contentAsJson(result) shouldBe Json.toJson(expected)
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

      import bindings.{
      GroupDescriptor,
      RoomDescriptor,
      ScheduleEntryDescriptor,
      LabworkDescriptor,
      EmployeeDescriptor
      }

      val courseID = UUID.randomUUID()
      val courseID2 = UUID.randomUUID()

      val labwork1 = Labwork("Label1", "Desc1", UUID.randomUUID(), courseID, UUID.randomUUID())
      val labwork2 = Labwork("Label2", "Desc2", UUID.randomUUID(), courseID2, UUID.randomUUID())
      val labwork3 = Labwork("Label3", "Desc3", UUID.randomUUID(), courseID, UUID.randomUUID())

      val group1 = Group("Label1", labwork1.id, Set.empty)
      val group2 = Group("Label2", labwork2.id, Set.empty)
      val group3 = Group("Label3", labwork3.id, Set.empty)
      val supervisor1 = Employee("systemid1", "lastname1", "firstname1", "email1", "status1")
      val supervisor2 = Employee("systemid2", "lastname2", "firstname2", "email2", "status2")
      val supervisor3 = Employee("systemid3", "lastname3", "firstname3", "email3", "status3")
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

      val sentry1 = ScheduleEntry(labwork1.id, start1, end1, date1, room1.id, Set(supervisor1.id), group1.id)
      val sentry2 = ScheduleEntry(labwork1.id, start1, end2, date2, room1.id, Set(supervisor1.id), group1.id)
      val sentry3 = ScheduleEntry(labwork2.id, start3, end3, date2, room2.id, Set(supervisor2.id), group2.id)
      val sentry4 = ScheduleEntry(labwork2.id, start2, end2, date3, room2.id, Set(supervisor2.id), group2.id)
      val sentry5 = ScheduleEntry(labwork1.id, start3, end2, date2, room1.id, Set(supervisor1.id), group2.id)
      val sentry6 = ScheduleEntry(labwork3.id, LocalTime.now plusHours 8, LocalTime.now plusHours 9, LocalDate.now plusDays 9, room1.id, Set(supervisor3.id), group3.id)


      realRepository addMany List(labwork1, labwork2, labwork3)
      realRepository addMany List(group1, group2, group3)
      realRepository addMany List(supervisor1, supervisor2)
      realRepository addMany List(room1, room2)
      realRepository addMany List(sentry1, sentry2, sentry3, sentry4, sentry5, sentry6)


      val requestForCourse = FakeRequest(
        GET,
        s"/courses/$courseID/labwork/${labwork1.id}/entries?course=$courseID"
      )

      val requestForLabwork = FakeRequest(
        GET,
        s"/courses/$courseID2/labwork/${labwork2.id}/entries?labwork=${labwork2.id}"
      )

      val requestForCourseAndLabwork = FakeRequest(
        GET,
        s"/courses/$courseID/labwork/${labwork1.id}/entries?course=$courseID&labwork=${labwork3.id}"
      )

      val requestForGroup = FakeRequest(
        GET,
        s"/courses/$courseID/labwork/${labwork1.id}/entries?group=${group1.id}"
      )

      val requestForSupervisor = FakeRequest(
        GET,
        s"/courses/$courseID/labwork/${labwork1.id}/entries?supervisor=${supervisor1.id}"
      )

      val requestGroupSupervisor = FakeRequest(
        GET,
        s"/courses/$courseID/labwork/${labwork1.id}/entries?group=${group2.id}&supervisor=${supervisor1.id}"
      )

      val requestForDate = FakeRequest(
        GET,
        s"/courses/$courseID/labwork/${labwork1.id}entries?date=$date2"
      )

      val requestForDateAndTime = FakeRequest(
        GET,
        s"/courses/$courseID/labwork/${labwork1.id}/entries?date=$date2&start=$start3"
      )

      val requestForMinMax = FakeRequest(
        GET,
        s"/courses/$courseID/labwork/${labwork1.id}/entries?dateRange=$date2,$date3"
      )

      val requestForGroupWithinDateRange = FakeRequest(
        GET,
        s"/courses/$courseID/labwork/${labwork1.id}/entries?group=${group1.id}&dateRange=$date2,$date4"
      )

      val requestForGroupWithinDateRangeAndSup = FakeRequest(
        GET,
        s"/courses/$courseID2/labwork/${labwork2.id}/entries?group=${group2.id}&dateRange=$date2,$date4&supervisor=${supervisor2.id}"
      )

      val resultForCourse = realController.allFrom(courseID.toString)(requestForCourse)
      val resultForLabwork = realController.allFrom(courseID2.toString)(requestForLabwork)
      val resultForCourseAndLabwork = realController.allFrom(courseID.toString)(requestForCourseAndLabwork)
      val resultForGroup = realController.allFrom(courseID.toString)(requestForGroup)
      val resultForSupervisor = realController.allFrom(courseID.toString)(requestForSupervisor)
      val resultForGroupSupervisor = realController.allFrom(courseID.toString)(requestGroupSupervisor)
      val resultForDate = realController.allFrom(courseID.toString)(requestForDate)
      val resultForDateAndTime = realController.allFrom(courseID.toString)(requestForDateAndTime)
      val resultForMinMax = realController.allFrom(courseID.toString)(requestForMinMax)
      val resultForGroupWithinDateRange = realController.allFrom(courseID.toString)(requestForGroupWithinDateRange)
      val resultForGroupWithinDateRangeAndSup = realController.allFrom(courseID2.toString)(requestForGroupWithinDateRangeAndSup)

      val valsForCourse = List(Json.toJson(sentry6), Json.toJson(sentry5), Json.toJson(sentry2), Json.toJson(sentry1))
      val valsForLabwork = List(Json.toJson(sentry4), Json.toJson(sentry3))
      val valsForCourseAndLabwork = List(Json.toJson(sentry6))
      val valsForGroup = List(Json.toJson(sentry2), Json.toJson(sentry1))
      val valsForSupervisor = List(Json.toJson(sentry5), Json.toJson(sentry2), Json.toJson(sentry1))
      val valsForGroupSupervisor = List(Json.toJson(sentry5))
      val valsForDate = List(Json.toJson(sentry5), Json.toJson(sentry2))
      val valsForDateAndTime = List(Json.toJson(sentry5))
      val valsForMinMax = List(Json.toJson(sentry5), Json.toJson(sentry2))
      val valsForGroupWithinDateRange = List(Json.toJson(sentry2))
      val valsForGroupWithinDateRangeAndSup = List(Json.toJson(sentry4), Json.toJson(sentry3))

      status(resultForCourse) shouldBe OK
      status(resultForLabwork) shouldBe OK
      status(resultForCourseAndLabwork) shouldBe OK
      status(resultForGroup) shouldBe OK
      status(resultForSupervisor) shouldBe OK
      status(resultForGroupSupervisor) shouldBe OK
      status(resultForDate) shouldBe OK
      status(resultForDateAndTime) shouldBe OK
      status(resultForMinMax) shouldBe OK
      status(resultForGroupWithinDateRange) shouldBe OK
      status(resultForGroupWithinDateRangeAndSup) shouldBe OK

      contentAsString(resultForCourse) shouldBe valsForCourse.mkString("")
      contentAsString(resultForLabwork) shouldBe valsForLabwork.mkString("")
      contentAsString(resultForCourseAndLabwork) shouldBe valsForCourseAndLabwork.mkString("")
      contentAsString(resultForGroup) shouldBe valsForGroup.mkString("")
      contentAsString(resultForSupervisor) shouldBe valsForSupervisor.mkString("")
      contentAsString(resultForGroupSupervisor) shouldBe valsForGroupSupervisor.mkString("")
      contentAsString(resultForDate) shouldBe valsForDate.mkString("")
      contentAsString(resultForDateAndTime) shouldBe valsForDateAndTime.mkString("")
      contentAsString(resultForMinMax) shouldBe valsForMinMax.mkString("")
      contentAsString(resultForGroupWithinDateRange) shouldBe valsForGroupWithinDateRange.mkString("")
      contentAsString(resultForGroupWithinDateRangeAndSup) shouldBe valsForGroupWithinDateRangeAndSup.mkString("")
    }
  }
}
