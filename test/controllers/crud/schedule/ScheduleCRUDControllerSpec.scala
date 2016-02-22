package controllers.crud.schedule

import java.util.UUID

import controllers.crud.{AbstractCRUDController, AbstractCRUDControllerSpec}
import models._
import models.schedule._
import models.semester.{Blacklist, Semester}
import models.users.Employee
import org.joda.time.{LocalDate, LocalTime}
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{JsValue, Json, Writes}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import services.{ScheduleService, ScheduleEntryG, ScheduleG, Conflict}
import utils.{Evaluation, Gen, LwmMimeType}

import scala.util.{Try, Failure, Success}

class ScheduleCRUDControllerSpec extends AbstractCRUDControllerSpec[ScheduleProtocol, Schedule] {
  override val entityToFail: Schedule = Schedule(Labwork.randomUUID, Set.empty[ScheduleEntry], Schedule.randomUUID)

  override val entityToPass: Schedule = Schedule(Labwork.randomUUID, Set.empty[ScheduleEntry], Schedule.randomUUID)

  import ops._
  import bindings.ScheduleBinding.scheduleBinder

  override val pointedGraph: PointedGraph[Sesame] = entityToPass.toPG

  override def entityTypeName: String = "schedule"

  val scheduleService = org.scalatest.mock.MockitoSugar.mock[ScheduleService]

  override val controller: AbstractCRUDController[ScheduleProtocol, Schedule] = new ScheduleCRUDController(repository, namespace, roleService, scheduleService) {

    override protected def fromInput(input: ScheduleProtocol, id: Option[UUID]): Schedule = entityToPass

    override protected def restrictedContext(moduleId: String): PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }

    override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }
  }

  override implicit val jsonWrites: Writes[Schedule] = Schedule.writes

  override val mimeType: LwmMimeType = LwmMimeType.scheduleV1Json

  override val inputJson: JsValue = Json.obj(
    "labwork" -> entityToPass.labwork,
    "entries" -> entityToPass.entries
  )

  val emptyVector = Vector.empty[ScheduleEntryG]
  
  "A ScheduleCRUDController also" should {

    "return empty list of scheduleG's when there are no competitive schedules" in {
      val semester = Semester("name", "start", "end", "exam", Blacklist.empty, Semester.randomUUID)
      val course = Course("label", "abbreviation", Employee.randomUUID, 1, Course.randomUUID)
      val assignmentPlan = AssignmentPlan(2, Set.empty[AssignmentEntry])
      val labwork = Labwork("label", "description", semester.id, course.id, Degree.randomUUID, assignmentPlan)

      when(repository.query(anyObject())).thenReturn(None)

      val result = ScheduleCRUDController.competitive(labwork.id, repository)

      result match {
        case Success(s) => s shouldBe empty
        case Failure(e) => fail(s"Unable to retrieve existing schedules: $e")
      }
    }

    "return scheduleG's when there are competitive schedules" in {
      val course1 = Course("label", "abbreviation", Employee.randomUUID, 1, Course.randomUUID)
      val course2 = Course("label", "abbreviation", Employee.randomUUID, 1, Course.randomUUID)
      val course3 = Course("label", "abbreviation", Employee.randomUUID, 1, Course.randomUUID)
      val semester1 = Semester("name", "start", "end", "exam", Blacklist.empty, Semester.randomUUID)
      val assignmentPlan = AssignmentPlan(2, Set.empty[AssignmentEntry])
      val labwork1 = Labwork("label", "description", semester1.id, course1.id, Degree.randomUUID, assignmentPlan)
      val labwork2 = Labwork("label", "description", semester1.id, course2.id, Degree.randomUUID, assignmentPlan)
      val labwork3 = Labwork("label", "description", semester1.id, course3.id, Degree.randomUUID, assignmentPlan)

      val groups = (0 until 3).map(n => Group(n.toString, Labwork.randomUUID, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), Group.randomUUID)).toSet

      val first = Schedule(labwork1.id, Set.empty[ScheduleEntry], Schedule.randomUUID)
      val second = Schedule(labwork2.id, Set.empty[ScheduleEntry], Schedule.randomUUID)
      val third = Schedule(labwork3.id, Set.empty[ScheduleEntry], Schedule.randomUUID)

      when(repository.query(anyObject())).thenReturn(Some(Map("schedules" -> List.empty)))
      when(repository.getMany[Schedule](anyObject())(anyObject())).thenReturn(Success(Vector(first, second)))
      when(repository.get[Group](anyObject(), anyObject())).thenReturn(Success(groups))

      val result = ScheduleCRUDController.competitive(labwork3.id, repository)

      result match {
        case Success(s) =>
          s should not be empty
          s.size shouldBe 2

          val schedules = s.map(sg => Schedule(sg.labwork, Set.empty[ScheduleEntry], sg.id))
          schedules.contains(first) shouldBe true
          schedules.contains(second) shouldBe true
          schedules.contains(third) shouldBe false
        case Failure(e) => fail(s"Unable to retrieve existing schedules: $e")
      }
    }

    "preview a schedule successfully when there are no competitive schedules" in {
      val plan = AssignmentPlan(2, Set(
        AssignmentEntry(1, Set.empty[EntryType]),
        AssignmentEntry(2, Set.empty[EntryType])
      ))
      val labwork = Labwork("", "", UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID(), plan)
      val timetable = Timetable(labwork.id, Set(
        TimetableEntry(UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID(), 1, LocalTime.now, LocalTime.now)
      ), LocalDate.now, Blacklist.empty, Timetable.randomUUID)
      val groups = (0 until 3).map(n => Group(n.toString, labwork.id, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), Group.randomUUID)).toSet
      val gen = Gen[ScheduleG, Conflict, Int](
        ScheduleG(labwork.id, emptyVector, Schedule.randomUUID),
        Evaluation[Conflict, Int](List.empty[Conflict], 0)
      )
      val schedule = {
        val entries = gen.elem.entries.map(e => ScheduleEntry(e.start, e.end, e.date, e.room, e.supervisor, e.group.id, e.id)).toSet
        Schedule(gen.elem.labwork, entries, gen.elem.id)
      }

      val request = FakeRequest(
        GET,
        s"/labworks/${labwork.id}/${entityTypeName}s/preview"
      )

      doReturn(Success(groups)).doReturn(Success(Set(timetable))).when(repository).get(anyObject(), anyObject())
      when(repository.get[Labwork](anyObject())(anyObject())).thenReturn(Success(Some(labwork)))
      when(repository.query(anyObject())).thenReturn(None)
      when(scheduleService.generate(anyObject(), anyObject(), anyObject(), anyObject())).thenReturn((gen, 0))

      val result = controller.asInstanceOf[ScheduleCRUDController].preview(labwork.id.toString)(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "OK",
        "schedule" -> Json.toJson(schedule),
        "number of conflicts" -> gen.evaluate.value
      )
    }

    "preview a schedule successfully although there are competitive schedules" in {
      val plan = AssignmentPlan(2, Set(
        AssignmentEntry(1, Set.empty[EntryType]),
        AssignmentEntry(2, Set.empty[EntryType])
      ))
      val labwork = Labwork("", "", UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID(), plan)
      val timetable = Timetable(labwork.id, Set(
        TimetableEntry(UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID(), 1, LocalTime.now, LocalTime.now)
      ), LocalDate.now, Blacklist.empty, Timetable.randomUUID)
      val groups = (0 until 3).map(n => Group(n.toString, labwork.id, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), Group.randomUUID)).toSet
      val gen = Gen[ScheduleG, Conflict, Int](
        ScheduleG(labwork.id, emptyVector, Schedule.randomUUID),
        Evaluation[Conflict, Int](List.empty[Conflict], 0)
      )
      val schedule = {
        val entries = gen.elem.entries.map(e => ScheduleEntry(e.start, e.end, e.date, e.room, e.supervisor, e.group.id, e.id)).toSet
        Schedule(gen.elem.labwork, entries, gen.elem.id)
      }

      val request = FakeRequest(
        GET,
        s"/labworks/${labwork.id}/${entityTypeName}s/preview"
      )

      doReturn(Success(groups)).doReturn(Success(Set(timetable))).doReturn(Success(Set.empty[Group])).when(repository).get(anyObject(), anyObject())
      when(repository.get[Labwork](anyObject())(anyObject())).thenReturn(Success(Some(labwork)))
      when(repository.query(anyObject())).thenReturn(Some(Map("schedules" -> List.empty)))
      when(repository.getMany[Schedule](anyObject())(anyObject())).thenReturn(Success(Vector.empty[Schedule]))
      when(scheduleService.generate(anyObject(), anyObject(), anyObject(), anyObject())).thenReturn((gen, 0))

      val result = controller.asInstanceOf[ScheduleCRUDController].preview(labwork.id.toString)(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "OK",
        "schedule" -> Json.toJson(schedule),
        "number of conflicts" -> gen.evaluate.value
      )
    }

    "preview a schedule successfully where conflicts are found" in {
      val plan = AssignmentPlan(2, Set(
        AssignmentEntry(1, Set.empty[EntryType]),
        AssignmentEntry(2, Set.empty[EntryType])
      ))
      val labwork = Labwork("", "", UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID(), plan)
      val timetable = Timetable(labwork.id, Set(
        TimetableEntry(UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID(), 1, LocalTime.now, LocalTime.now)
      ), LocalDate.now, Blacklist.empty, Timetable.randomUUID)
      val groups = (0 until 3).map(n => Group(n.toString, labwork.id, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), Group.randomUUID)).toSet
      val gen = Gen[ScheduleG, Conflict, Int](
        ScheduleG(labwork.id, emptyVector, Schedule.randomUUID),
        Evaluation[Conflict, Int](List(
          Conflict(
            ScheduleEntryG(LocalTime.now, LocalTime.now, LocalDate.now, UUID.randomUUID(), UUID.randomUUID(), groups.head, UUID.randomUUID()),
            groups.head.members.toVector.take(1),
            groups.head
          )
        ), 1)
      )
      val schedule = {
        val entries = gen.elem.entries.map(e => ScheduleEntry(e.start, e.end, e.date, e.room, e.supervisor, e.group.id, e.id)).toSet
        Schedule(gen.elem.labwork, entries, gen.elem.id)
      }

      val request = FakeRequest(
        GET,
        s"/labworks/${labwork.id}/${entityTypeName}s/preview"
      )

      doReturn(Success(groups)).doReturn(Success(Set(timetable))).doReturn(Success(Set.empty[Group])).when(repository).get(anyObject(), anyObject())
      when(repository.get[Labwork](anyObject())(anyObject())).thenReturn(Success(Some(labwork)))
      when(repository.query(anyObject())).thenReturn(Some(Map("schedules" -> List.empty)))
      when(repository.getMany[Schedule](anyObject())(anyObject())).thenReturn(Success(Vector.empty[Schedule]))
      when(scheduleService.generate(anyObject(), anyObject(), anyObject(), anyObject())).thenReturn((gen, 0))

      val result = controller.asInstanceOf[ScheduleCRUDController].preview(labwork.id.toString)(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "OK",
        "schedule" -> Json.toJson(schedule),
        "number of conflicts" -> gen.evaluate.value
      )
    }

    "not preview a schedule when assignment plan is empty" in {
      val plan = AssignmentPlan(0, Set.empty[AssignmentEntry])
      val labwork = Labwork("", "", UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID(), plan)
      val timetable = Timetable(labwork.id, Set(
        TimetableEntry(UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID(), 1, LocalTime.now, LocalTime.now)
      ), LocalDate.now, Blacklist.empty, Timetable.randomUUID)
      val groups = (0 until 3).map(n => Group(n.toString, labwork.id, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), Group.randomUUID)).toSet
      val gen = Gen[ScheduleG, Conflict, Int](
        ScheduleG(labwork.id, emptyVector, Schedule.randomUUID),
        Evaluation[Conflict, Int](List.empty[Conflict], 0)
      )
      val schedule = {
        val entries = gen.elem.entries.map(e => ScheduleEntry(e.start, e.end, e.date, e.room, e.supervisor, e.group.id, e.id)).toSet
        Schedule(gen.elem.labwork, entries, gen.elem.id)
      }

      val request = FakeRequest(
        GET,
        s"/labworks/${labwork.id}/${entityTypeName}s/preview"
      )

      doReturn(Success(groups)).doReturn(Success(Set(timetable))).when(repository).get(anyObject(), anyObject())
      when(repository.get[Labwork](anyObject())(anyObject())).thenReturn(Success(Some(labwork)))
      when(repository.query(anyObject())).thenReturn(None)
      when(scheduleService.generate(anyObject(), anyObject(), anyObject(), anyObject())).thenReturn((gen, 0))

      val result = controller.asInstanceOf[ScheduleCRUDController].preview(labwork.id.toString)(request)

      status(result) shouldBe NOT_FOUND
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "message" -> "No such element..."
      )
    }

    "not preview a schedule when timetable is empty" in {
      val plan = AssignmentPlan(2, Set(
        AssignmentEntry(1, Set.empty[EntryType]),
        AssignmentEntry(2, Set.empty[EntryType])
      ))
      val labwork = Labwork("", "", UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID(), plan)
      val timetable = Timetable(labwork.id, Set.empty[TimetableEntry], LocalDate.now, Blacklist.empty, Timetable.randomUUID)
      val groups = (0 until 3).map(n => Group(n.toString, labwork.id, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID), Group.randomUUID)).toSet
      val gen = Gen[ScheduleG, Conflict, Int](
        ScheduleG(labwork.id, emptyVector, Schedule.randomUUID),
        Evaluation[Conflict, Int](List.empty[Conflict], 0)
      )
      val schedule = {
        val entries = gen.elem.entries.map(e => ScheduleEntry(e.start, e.end, e.date, e.room, e.supervisor, e.group.id, e.id)).toSet
        Schedule(gen.elem.labwork, entries, gen.elem.id)
      }

      val request = FakeRequest(
        GET,
        s"/labworks/${labwork.id}/${entityTypeName}s/preview"
      )

      doReturn(Success(groups)).doReturn(Success(Set(timetable))).when(repository).get(anyObject(), anyObject())
      when(repository.get[Labwork](anyObject())(anyObject())).thenReturn(Success(Some(labwork)))
      when(repository.query(anyObject())).thenReturn(None)
      when(scheduleService.generate(anyObject(), anyObject(), anyObject(), anyObject())).thenReturn((gen, 0))

      val result = controller.asInstanceOf[ScheduleCRUDController].preview(labwork.id.toString)(request)

      status(result) shouldBe NOT_FOUND
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "message" -> "No such element..."
      )
    }

    "not preview a schedule when groups are empty" in {
      val plan = AssignmentPlan(2, Set(
        AssignmentEntry(1, Set.empty[EntryType]),
        AssignmentEntry(2, Set.empty[EntryType])
      ))
      val labwork = Labwork("", "", UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID(), plan)
      val timetable = Timetable(labwork.id, Set(
        TimetableEntry(UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID(), 1, LocalTime.now, LocalTime.now)
      ), LocalDate.now, Blacklist.empty, Timetable.randomUUID)
      val gen = Gen[ScheduleG, Conflict, Int](
        ScheduleG(labwork.id, emptyVector, Schedule.randomUUID),
        Evaluation[Conflict, Int](List.empty[Conflict], 0)
      )
      val schedule = {
        val entries = gen.elem.entries.map(e => ScheduleEntry(e.start, e.end, e.date, e.room, e.supervisor, e.group.id, e.id)).toSet
        Schedule(gen.elem.labwork, entries, gen.elem.id)
      }

      val request = FakeRequest(
        GET,
        s"/labworks/${labwork.id}/${entityTypeName}s/preview"
      )

      doReturn(Success(Set.empty[Group])).doReturn(Success(Set(timetable))).when(repository).get(anyObject(), anyObject())
      when(repository.get[Labwork](anyObject())(anyObject())).thenReturn(Success(Some(labwork)))
      when(repository.query(anyObject())).thenReturn(None)
      when(scheduleService.generate(anyObject(), anyObject(), anyObject(), anyObject())).thenReturn((gen, 0))

      val result = controller.asInstanceOf[ScheduleCRUDController].preview(labwork.id.toString)(request)

      status(result) shouldBe NOT_FOUND
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "message" -> "No such element..."
      )
    }

    "not preview a schedule when db errors occur" in {
      val labwork = Labwork.randomUUID

      val request = FakeRequest(
        GET,
        s"/labworks/$labwork/${entityTypeName}s/preview"
      )

      val exception = new Exception("Oops, something went wrong")
      doReturn(Failure(exception)).doReturn(Failure(exception)).when(repository).get(anyObject(), anyObject())
      when(repository.get[Labwork](anyObject())(anyObject())).thenReturn(Failure(exception))
      when(repository.query(anyObject())).thenReturn(None)

      val result = controller.asInstanceOf[ScheduleCRUDController].preview(labwork.toString)(request)

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "errors" -> exception.getMessage
      )
    }
  }
}
