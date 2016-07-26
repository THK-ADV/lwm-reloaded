package controllers.schedule

import java.util.UUID

import base.TestBaseDefinition
import models._
import models.labwork._
import models.semester.Semester
import models.users.{Employee, User}
import org.joda.time.{DateTime, LocalDate, LocalTime}
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.openrdf.model.Value
import org.openrdf.model.impl.ValueFactoryImpl
import org.scalatest.WordSpec
import org.scalatest.mock.MockitoSugar.mock
import org.w3.banana.sesame.SesameModule
import play.api.libs.json.{JsValue, Json, Writes}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import services._
import store.sparql.{QueryEngine, QueryExecutor, SelectClause}
import store.{Namespace, SesameRepository}
import utils.{Evaluation, Gen, LwmMimeType}

import scala.util.{Failure, Success}

class ScheduleControllerSpec extends WordSpec with TestBaseDefinition with SesameModule {

  val scheduleService = mock[ScheduleService]
  val repository = mock[SesameRepository]
  val sessionService = mock[SessionHandlingService]
  val roleService = mock[RoleService]
  val namespace = Namespace("test://lwm.gm.fh-koeln.de")
  val factory = ValueFactoryImpl.getInstance()
  val qe = mock[QueryExecutor[SelectClause]]
  val query = QueryEngine.empty(qe)

  val labworkToPass = Labwork("label to pass", "desc to pass", UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID())
  val labworkToFail = Labwork("label to fail", "desc to fail", UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID())

  val roomToPass = Room("room to pass", "desc to pass")
  val roomToFail = Room("room to fail", "desc to fail")

  val supervisorToPass = Employee("systemId to pass", "last name to pass", "first name to pass", "email to pass", "status to pass")
  val supervisorToFail = Employee("systemId to fail", "last name to fail", "first name to fail", "email to fail", "status to fail")

  val groupToPass = Group("group to pass", labworkToPass.id, Set(UUID.randomUUID(), UUID.randomUUID()))
  val groupToFail = Group("group to fail", labworkToFail.id, Set(UUID.randomUUID(), UUID.randomUUID()))

  val entriesToPass = (0 until 10).map(n =>
    ScheduleEntry(
      labworkToPass.id,
      LocalTime.now.plusHours(n),
      LocalTime.now.plusHours(n),
      LocalDate.now.plusWeeks(n),
      roomToPass.id,
      supervisorToPass.id,
      groupToPass.id
    )
  ).toSet
  val entriesToFail = (0 until 10).map(n =>
    ScheduleEntry(
      labworkToFail.id,
      LocalTime.now.plusHours(n),
      LocalTime.now.plusHours(n),
      LocalDate.now.plusWeeks(n),
      roomToFail.id,
      supervisorToFail.id,
      groupToFail.id
    )
  ).toSet

  val entityToFail: Schedule = Schedule(labworkToFail.id, entriesToFail)

  val entityToPass: Schedule = Schedule(labworkToPass.id, entriesToPass)

  val controller = new ScheduleController(repository, sessionService, namespace, roleService, scheduleService) {

    override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }

    override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }
  }

  implicit val jsonWrites: Writes[Schedule] = Schedule.writes

  val mimeType: LwmMimeType = LwmMimeType.scheduleV1Json

  val inputJson: JsValue = Json.obj(
    "labwork" -> entityToPass.labwork,
    "entries" -> entityToPass.entries
  )

  val emptyVector = Vector.empty[ScheduleEntryG]

  when(qe.parse(anyObject())).thenReturn(sparqlOps.parseSelect("SELECT * where {}"))
  when(qe.execute(anyObject())).thenReturn(Success(Map.empty[String, List[Value]]))

  "A ScheduleCRUDController also" should {

    "return empty list of scheduleG's when there are no competitive schedules" in {
      val semester = Semester("semester1", "abbrev", LocalDate.now, LocalDate.now, LocalDate.now)
      val course = Course("label", "desc", "abbreviation", User.randomUUID, 1)
      val labwork = Labwork("label", "description", semester.id, course.id, Degree.randomUUID)

      when(repository.prepareQuery(anyObject())).thenReturn(query)
      when(qe.execute(anyObject())).thenReturn(Success(Map("schedules" -> List.empty)))
      when(repository.getMany[Schedule](anyObject())(anyObject())).thenReturn(Success(Set.empty[Schedule]))

      val result = ScheduleController.competitive(labwork.id, repository)

      result match {
        case Success(s) => s shouldBe empty
        case Failure(e) => fail(s"Unable to retrieve existing schedules: $e")
      }
    }

    "return scheduleG's when there are competitive schedules" in {
      val course1 = Course("label", "desc", "abbreviation", User.randomUUID, 1)
      val course2 = Course("label", "desc", "abbreviation", User.randomUUID, 1)
      val course3 = Course("label", "desc", "abbreviation", User.randomUUID, 1)
      val semester1 = Semester("semester1", "abbrev", LocalDate.now, LocalDate.now, LocalDate.now)
      val labwork1 = Labwork("label", "description", semester1.id, course1.id, Degree.randomUUID)
      val labwork2 = Labwork("label", "description", semester1.id, course2.id, Degree.randomUUID)
      val labwork3 = Labwork("label", "description", semester1.id, course3.id, Degree.randomUUID)

      val groups = (0 until 3).map(n => Group(n.toString, Labwork.randomUUID, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID))).toSet

      val first = Schedule(labwork1.id, Set.empty[ScheduleEntry])
      val second = Schedule(labwork2.id, Set.empty[ScheduleEntry])
      val third = Schedule(labwork3.id, Set.empty[ScheduleEntry])

      when(repository.prepareQuery(anyObject())).thenReturn(query)
      when(qe.execute(anyObject())).thenReturn(Success(Map("schedules" -> List.empty)))
      when(repository.getMany[Schedule](anyObject())(anyObject())).thenReturn(Success(Set(first, second)))
      when(repository.getAll[Group](anyObject())).thenReturn(Success(groups))

      val result = ScheduleController.competitive(labwork3.id, repository)

      result match {
        case Success(s) =>
          s should not be empty
          s.size shouldBe 2

          val schedules = s.map(sg => Schedule(sg.labwork, Set.empty[ScheduleEntry], None, sg.id))
          schedules.contains(first) shouldBe true
          schedules.contains(second) shouldBe true
          schedules.contains(third) shouldBe false
        case Failure(e) => fail(s"Unable to retrieve existing schedules: $e")
      }
    }

    "preview a schedule successfully when there are no competitive schedules" in {
      val lecturer = Employee("systemid", "lastname", "firstname", "email", "lecturer")
      val semester = Semester("", "", LocalDate.now, LocalDate.now, LocalDate.now)
      val course = CourseAtom("", "", "", lecturer, 2, None, Course.randomUUID)
      val degree = Degree("degree", "abbrev")
      val labwork = LabworkAtom("", "", semester, course, degree, false, false, None, Labwork.randomUUID)
      val plan = AssignmentPlan(labwork.id, 2, 2, Set(AssignmentEntry(0, "A", Set.empty)))
      val timetable = Timetable(labwork.id, Set(
        TimetableEntry(UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID(), 1, LocalTime.now, LocalTime.now)
      ), LocalDate.now, Set.empty[DateTime])

      val groups = (0 until 3).map(n => Group(n.toString, labwork.id, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID))).toSet

      val gen = Gen[ScheduleG, Conflict, Int](
        ScheduleG(labwork.id, emptyVector, Schedule.randomUUID),
        Evaluation[Conflict, Int](List.empty[Conflict], 0)
      )

      val schedule = {
        val entries = gen.elem.entries.map(e => ScheduleEntry(labwork.id, e.start, e.end, e.date, e.room, e.supervisor, e.group.id)).toSet
        Schedule(gen.elem.labwork, entries, None, gen.elem.id)
      }

      val request = FakeRequest(
        GET,
        "/schedules/preview"
      )

      doReturn(Success(Some(labwork)))
        .when(repository).get(anyObject())(anyObject())

      doReturn(Success(groups))
        .doReturn(Success(Set(timetable)))
        .doReturn(Success(Set(plan)))
        .when(repository).getAll(anyObject())

      when(repository.prepareQuery(anyObject())).thenReturn(query)
      when(scheduleService.generate(anyObject(), anyObject(), anyObject(), anyObject(), anyObject())).thenReturn((gen, 0))

      val result = controller.preview(labwork.course.toString, labwork.id.toString)(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "OK",
        "schedule" -> Json.toJson(schedule),
        "number of conflicts" -> gen.evaluate.value
      )
    }

    "preview a schedule successfully although there are competitive schedules" in {
      val lecturer = Employee("systemid", "lastname", "firstname", "email", "lecturer")
      val semester = Semester("", "", LocalDate.now, LocalDate.now, LocalDate.now)
      val course = CourseAtom("", "", "", lecturer, 2, None, Course.randomUUID)
      val degree = Degree("degree", "abbrev")
      val labwork = LabworkAtom("", "", semester, course, degree, false, false, None, Labwork.randomUUID)
      val plan = AssignmentPlan(labwork.id, 2, 2, Set(AssignmentEntry(0, "A", Set.empty)))
      val timetable = Timetable(labwork.id, Set(
        TimetableEntry(UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID(), 1, LocalTime.now, LocalTime.now)
      ), LocalDate.now, Set.empty[DateTime])
      val groups = (0 until 3).map(n => Group(n.toString, labwork.id, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID))).toSet
      val gen = Gen[ScheduleG, Conflict, Int](
        ScheduleG(labwork.id, emptyVector, Schedule.randomUUID),
        Evaluation[Conflict, Int](List.empty[Conflict], 0)
      )
      val schedule = {
        val entries = gen.elem.entries.map(e => ScheduleEntry(labwork.id, e.start, e.end, e.date, e.room, e.supervisor, e.group.id)).toSet
        Schedule(gen.elem.labwork, entries, None, gen.elem.id)
      }

      val request = FakeRequest(
        GET,
        "/schedules/preview"
      )

      doReturn(Success(Some(labwork)))
        .when(repository).get(anyObject())(anyObject())

      doReturn(Success(groups))
        .doReturn(Success(Set(timetable)))
        .doReturn(Success(Set(plan)))
        .doReturn(Success(Set.empty[Group]))
        .when(repository).getAll(anyObject())

      when(repository.prepareQuery(anyObject())).thenReturn(query)
      when(qe.execute(anyObject())).thenReturn(Success(Map("schedules" -> List.empty)))
      when(repository.getMany[Schedule](anyObject())(anyObject())).thenReturn(Success(Set.empty[Schedule]))
      when(scheduleService.generate(anyObject(), anyObject(), anyObject(), anyObject(), anyObject())).thenReturn((gen, 0))

      val result = controller.preview(labwork.course.toString, labwork.id.toString)(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "OK",
        "schedule" -> Json.toJson(schedule),
        "number of conflicts" -> gen.evaluate.value
      )
    }

    "preview a schedule successfully where conflicts are found" in {
      val lecturer = Employee("systemid", "lastname", "firstname", "email", "lecturer")
      val semester = Semester("", "", LocalDate.now, LocalDate.now, LocalDate.now)
      val course = CourseAtom("", "", "", lecturer, 2, None, Course.randomUUID)
      val degree = Degree("degree", "abbrev")
      val labwork = LabworkAtom("", "", semester, course, degree, false, false, None, Labwork.randomUUID)
      val plan = AssignmentPlan(labwork.id, 2, 2, Set(AssignmentEntry(0, "A", Set.empty)))
      val timetable = Timetable(labwork.id, Set(
        TimetableEntry(UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID(), 1, LocalTime.now, LocalTime.now)
      ), LocalDate.now, Set.empty[DateTime])
      val groups = (0 until 3).map(n => Group(n.toString, labwork.id, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID))).toSet
      val gen = Gen[ScheduleG, Conflict, Int](
        ScheduleG(labwork.id, emptyVector, Schedule.randomUUID),
        Evaluation[Conflict, Int](List(
          Conflict(
            ScheduleEntryG(LocalTime.now, LocalTime.now, LocalDate.now, UUID.randomUUID(), UUID.randomUUID(), groups.head),
            groups.head.members.toVector.take(1),
            groups.head
          )
        ), 1)
      )
      val schedule = {
        val entries = gen.elem.entries.map(e => ScheduleEntry(labwork.id, e.start, e.end, e.date, e.room, e.supervisor, e.group.id)).toSet
        Schedule(gen.elem.labwork, entries, None, gen.elem.id)
      }

      val request = FakeRequest(
        GET,
        "/schedules/preview"
      )

      doReturn(Success(Some(labwork)))
        .when(repository).get(anyObject())(anyObject())

      doReturn(Success(groups))
        .doReturn(Success(Set(timetable)))
        .doReturn(Success(Set(plan)))
        .doReturn(Success(Set.empty[Group]))
        .when(repository).getAll(anyObject())

      when(repository.prepareQuery(anyObject())).thenReturn(query)
      when(qe.execute(anyObject())).thenReturn(Success(Map("schedules" -> List.empty)))
      when(repository.getMany[Schedule](anyObject())(anyObject())).thenReturn(Success(Set.empty[Schedule]))
      when(scheduleService.generate(anyObject(), anyObject(), anyObject(), anyObject(), anyObject())).thenReturn((gen, 0))

      val result = controller.preview(labwork.course.toString, labwork.id.toString)(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "OK",
        "schedule" -> Json.toJson(schedule),
        "number of conflicts" -> gen.evaluate.value
      )
    }

    "not preview a schedule when assignment plan is empty" in {
      val lecturer = Employee("systemid", "lastname", "firstname", "email", "lecturer")
      val semester = Semester("", "", LocalDate.now, LocalDate.now, LocalDate.now)
      val course = CourseAtom("", "", "", lecturer, 2, None, Course.randomUUID)
      val degree = Degree("degree", "abbrev")
      val labwork = LabworkAtom("", "", semester, course, degree, false, false, None, Labwork.randomUUID)
      val timetable = Timetable(labwork.id, Set(
        TimetableEntry(UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID(), 1, LocalTime.now, LocalTime.now)
      ), LocalDate.now, Set.empty[DateTime])
      val groups = (0 until 3).map(n => Group(n.toString, labwork.id, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID))).toSet
      val gen = Gen[ScheduleG, Conflict, Int](
        ScheduleG(labwork.id, emptyVector, Schedule.randomUUID),
        Evaluation[Conflict, Int](List.empty[Conflict], 0)
      )

      val request = FakeRequest(
        GET,
        "schedules/preview"
      )

      doReturn(Success(Some(labwork)))
        .when(repository).get(anyObject())(anyObject())

      doReturn(Success(groups))
        .doReturn(Success(Set(timetable)))
        .doReturn(Success(Set(AssignmentPlan.empty)))
        .when(repository).getAll(anyObject())

      when(repository.prepareQuery(anyObject())).thenReturn(query)
      when(scheduleService.generate(anyObject(), anyObject(), anyObject(), anyObject(), anyObject())).thenReturn((gen, 0))

      val result = controller.preview(labwork.course.toString, labwork.id.toString)(request)

      status(result) shouldBe NOT_FOUND
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "message" -> "No such element..."
      )
    }

    "not preview a schedule when timetable is empty" in {
      val lecturer = Employee("systemid", "lastname", "firstname", "email", "lecturer")
      val semester = Semester("", "", LocalDate.now, LocalDate.now, LocalDate.now)
      val course = CourseAtom("", "", "", lecturer, 2, None, Course.randomUUID)
      val degree = Degree("degree", "abbrev")
      val labwork = LabworkAtom("", "", semester, course, degree, false, false, None, Labwork.randomUUID)
      val plan = AssignmentPlan(labwork.id, 2, 2, Set(AssignmentEntry(0, "A", Set.empty)))
      val timetable = Timetable(labwork.id, Set.empty[TimetableEntry], LocalDate.now, Set.empty[DateTime])
      val groups = (0 until 3).map(n => Group(n.toString, labwork.id, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID))).toSet
      val gen = Gen[ScheduleG, Conflict, Int](
        ScheduleG(labwork.id, emptyVector, Schedule.randomUUID),
        Evaluation[Conflict, Int](List.empty[Conflict], 0)
      )

      val request = FakeRequest(
        GET,
        "schedules/preview"
      )

      doReturn(Success(Some(labwork)))
        .when(repository).get(anyObject())(anyObject())

      doReturn(Success(groups))
        .doReturn(Success(Set(timetable)))
        .doReturn(Success(Set(plan)))
        .when(repository).getAll(anyObject())

      when(repository.prepareQuery(anyObject())).thenReturn(query)
      when(scheduleService.generate(anyObject(), anyObject(), anyObject(), anyObject(), anyObject())).thenReturn((gen, 0))

      val result = controller.preview(labwork.course.toString, labwork.id.toString)(request)

      status(result) shouldBe NOT_FOUND
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "message" -> "No such element..."
      )
    }

    "not preview a schedule when groups are empty" in {
      val lecturer = Employee("systemid", "lastname", "firstname", "email", "lecturer")
      val semester = Semester("", "", LocalDate.now, LocalDate.now, LocalDate.now)
      val course = CourseAtom("", "", "", lecturer, 2, None, Course.randomUUID)
      val degree = Degree("degree", "abbrev")
      val labwork = LabworkAtom("", "", semester, course, degree, false, false, None, Labwork.randomUUID)
      val plan = AssignmentPlan(labwork.id, 2, 2, Set(AssignmentEntry(0, "A", Set.empty)))
      val timetable = Timetable(labwork.id, Set(
        TimetableEntry(UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID(), 1, LocalTime.now, LocalTime.now)
      ), LocalDate.now, Set.empty[DateTime])
      val gen = Gen[ScheduleG, Conflict, Int](
        ScheduleG(labwork.id, emptyVector, Schedule.randomUUID),
        Evaluation[Conflict, Int](List.empty[Conflict], 0)
      )

      val request = FakeRequest(
        GET,
        "schedules/preview"
      )

      doReturn(Success(Some(labwork)))
        .when(repository).get(anyObject())(anyObject())

      doReturn(Success(Set.empty[Group]))
        .doReturn(Success(Set(timetable)))
        .doReturn(Success(Set(plan)))
        .when(repository).getAll(anyObject())

      when(repository.prepareQuery(anyObject())).thenReturn(query)
      when(scheduleService.generate(anyObject(), anyObject(), anyObject(), anyObject(), anyObject())).thenReturn((gen, 0))

      val result = controller.preview(labwork.course.toString, labwork.id.toString)(request)

      status(result) shouldBe NOT_FOUND
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "message" -> "No such element..."
      )
    }

    "not preview a schedule when db errors occur" in {
      val lecturer = Employee("systemid", "lastname", "firstname", "email", "lecturer")
      val semester = Semester("", "", LocalDate.now, LocalDate.now, LocalDate.now)
      val course = CourseAtom("", "", "", lecturer, 2, None, Course.randomUUID)
      val degree = Degree("degree", "abbrev")
      val labwork = LabworkAtom("", "", semester, course, degree, false, false, None, Labwork.randomUUID)


      val request = FakeRequest(
        GET,
        "schedules/preview"
      )

      val exception = new Exception("Oops, something went wrong")
      doReturn(Success(Some(labwork)))
        .when(repository).get(anyObject())(anyObject())

      doReturn(Failure(exception))
        .doReturn(Failure(exception))
        .doReturn(Success(Set(AssignmentPlan.empty)))
        .when(repository).getAll(anyObject())

      when(repository.prepareQuery(anyObject())).thenReturn(query)

      val result = controller.preview(course.id.toString, labwork.id.toString)(request)

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "errors" -> exception.getMessage
      )
    }
  }
}
