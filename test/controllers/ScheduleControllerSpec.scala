package controllers

import java.util.UUID

import base.TestBaseDefinition
import controllers.GroupCRUDController._
import models._
import org.joda.time.{DateTime, LocalDate, LocalTime}
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.scalatest.WordSpec
import org.scalatest.mock.MockitoSugar.mock
import org.w3.banana.sesame.SesameModule
import play.api.libs.json.{JsValue, Json, Writes}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import services._
import store.{Namespace, SesameRepository}
import utils.{Evaluation, Gen, LwmMimeType}

import scala.util.Success

class ScheduleControllerSpec extends WordSpec with TestBaseDefinition with SesameModule {

  val scheduleService = mock[ScheduleService]
  val repository = mock[SesameRepository]
  val sessionService = mock[SessionHandlingService]
  val roleService = mock[RoleService]
  val groupService = mock[GroupService]
  val namespace = Namespace("test://lwm.gm.fh-koeln.de")

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
      Set(supervisorToPass.id),
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
      Set(supervisorToFail.id),
      groupToFail.id
    )
  ).toSet

  val entityToFail: Schedule = Schedule(labworkToFail.id, entriesToFail)

  val entityToPass: Schedule = Schedule(labworkToPass.id, entriesToPass)

  val controller = new ScheduleController(repository, sessionService, namespace, roleService, scheduleService, groupService) {

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

  val previewRequest = FakeRequest(
    GET,
    s"/schedules/preview?$countAttribute=8"
  )

  val lecturer = Employee("systemid", "lastname", "firstname", "email", "lecturer")
  val semester = Semester("", "", LocalDate.now, LocalDate.now, LocalDate.now)
  val course = CourseAtom("", "", "", lecturer, 2, None, Course.randomUUID)
  val degree = Degree("degree", "abbrev")
  val labwork = LabworkAtom("", "", semester, course, degree, subscribable = false, published = false, None, Labwork.randomUUID)
  val plan = AssignmentPlan(labwork.id, 2, 2, Set(AssignmentEntry(0, "A", Set.empty)))
  val timetable = Timetable(labwork.id, Set(
    TimetableEntry(Set(UUID.randomUUID()), UUID.randomUUID(), 1, LocalTime.now, LocalTime.now)
  ), LocalDate.now, Set.empty[DateTime])
  val groups = (0 until 3).map(n => Group(n.toString, labwork.id, Set(UUID.randomUUID, UUID.randomUUID, UUID.randomUUID))).toSet

  val randomAtom = {
    val entries = (0 until 10).map(i => ScheduleEntryAtom(labwork, LocalTime.now.plusHours(i), LocalTime.now.plusHours(i + 1), LocalDate.now.plusDays(i), Room.default, Set(Employee.default), Group.empty, None, UUID.randomUUID)).toSet
    ScheduleAtom(labwork, entries, None, UUID.randomUUID)
  }

  def schedule(gen: Gen[ScheduleG, Conflict, Int]): Schedule = {
    val entries = gen.elem.entries.map(e => ScheduleEntry(labwork.id, e.start, e.end, e.date, e.room, e.supervisor, e.group.id)).toSet
    Schedule(gen.elem.labwork, entries, None, gen.elem.id)
  }

  private def assumptions(gen: Gen[ScheduleG, Conflict, Int], comps: Boolean = true) = {
    val comp = if (comps) Set(randomAtom) else Set.empty[ScheduleAtom]

    when(repository.get[LabworkAtom](anyObject())(anyObject())).thenReturn(Success(Some(labwork)))
    doReturn(Success(groups)).when(groupService).groupBy(anyObject(), anyObject())

    doReturn(Success(Set(timetable)))
      .doReturn(Success(Set(plan)))
      .doReturn(Success(comp))
      .when(repository).getAll(anyObject())

    when(scheduleService.competitive(anyObject(), anyObject())).thenReturn(comp.map(scheduleG))
    when(scheduleService.generate(anyObject(), anyObject(), anyObject(), anyObject(), anyObject(), anyObject(), anyObject(), anyObject())).thenReturn((gen, 0))
  }

  "A ScheduleCRUDController also" should {

    "preview a schedule successfully when there are no competitive schedules" in {
      val gen = Gen[ScheduleG, Conflict, Int](
        ScheduleG(labwork.id, emptyVector, Schedule.randomUUID),
        Evaluation[Conflict, Int](List.empty[Conflict], 0)
      )

      assumptions(gen, comps = false)

      val result = controller.preview(labwork.course.toString, labwork.id.toString)(previewRequest)

      status(result) shouldBe OK
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "OK",
        "schedule" -> Json.toJson(schedule(gen)),
        "number of conflicts" -> gen.evaluate.value
      )
    }

    "preview a schedule successfully although there are competitive schedules" in {
      val gen = Gen[ScheduleG, Conflict, Int](
        ScheduleG(labwork.id, emptyVector, Schedule.randomUUID),
        Evaluation[Conflict, Int](List.empty[Conflict], 0)
      )

      assumptions(gen)

      val result = controller.preview(labwork.course.toString, labwork.id.toString)(previewRequest)

      status(result) shouldBe OK
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "OK",
        "schedule" -> Json.toJson(schedule(gen)),
        "number of conflicts" -> gen.evaluate.value
      )
    }

    "preview a schedule successfully where conflicts are found" in {
      val gen = Gen[ScheduleG, Conflict, Int](
        ScheduleG(labwork.id, emptyVector, Schedule.randomUUID),
        Evaluation[Conflict, Int](List(
          Conflict(
            ScheduleEntryG(LocalTime.now, LocalTime.now, LocalDate.now, UUID.randomUUID(), Set(UUID.randomUUID()), groups.head),
            groups.head.members.toVector.take(1),
            groups.head
          )
        ), 1)
      )

      assumptions(gen)

      val result = controller.preview(labwork.course.toString, labwork.id.toString)(previewRequest)

      status(result) shouldBe OK
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "OK",
        "schedule" -> Json.toJson(schedule(gen)),
        "number of conflicts" -> gen.evaluate.value
      )
    }
  }

  def scheduleG(scheduleAtom: ScheduleAtom): ScheduleG = {
    ScheduleG(scheduleAtom.labwork.id, scheduleAtom.entries.map(a => ScheduleEntryG(a.start, a.end, a.date, a.room.id, a.supervisor.map(_.id), a.group)).toVector, scheduleAtom.id)
  }
}
