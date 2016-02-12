package controllers.crud.schedule

import java.util.UUID

import controllers.crud.{AbstractCRUDController, AbstractCRUDControllerSpec}
import models._
import models.schedule._
import models.semester.{Blacklist, Semester}
import models.users.Employee
import org.joda.time.DateTime
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{JsValue, Json, Writes}
import services.ScheduleGenesisService
import utils.LwmMimeType

import scala.util.{Failure, Success}

class ScheduleCRUDControllerSpec extends AbstractCRUDControllerSpec[ScheduleProtocol, Schedule] {
  override val entityToFail: Schedule = Schedule(Labwork.randomUUID, Set.empty[ScheduleEntry], Schedule.randomUUID)

  override val entityToPass: Schedule = Schedule(Labwork.randomUUID, Set.empty[ScheduleEntry], Schedule.randomUUID)

  import ops._
  import bindings.ScheduleBinding.scheduleBinder

  override val pointedGraph: PointedGraph[Sesame] = entityToPass.toPG

  override def entityTypeName: String = "schedule"

  val scheduleGenesisService = org.scalatest.mock.MockitoSugar.mock[ScheduleGenesisService]

  override val controller: AbstractCRUDController[ScheduleProtocol, Schedule] = new ScheduleCRUDController(repository, namespace, roleService, scheduleGenesisService) {

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
      val semester2 = Semester("name", "start", "end", "exam", Blacklist.empty, Semester.randomUUID)
      val semester3 = Semester("name", "start", "end", "exam", Blacklist.empty, Semester.randomUUID)

      val assignmentPlan = AssignmentPlan(2, Set.empty[AssignmentEntry])
      val labwork1 = Labwork("label", "description", semester1.id, course1.id, Degree.randomUUID, assignmentPlan)
      val labwork2 = Labwork("label", "description", semester1.id, course2.id, Degree.randomUUID, assignmentPlan)
      val labwork3 = Labwork("label", "description", semester1.id, course3.id, Degree.randomUUID, assignmentPlan)

      val entries = (0 until 6).map(n => TimetableEntry(Employee.randomUUID, Room.randomUUID, Degree.randomUUID, DateTime.now, DateTime.now, DateTime.now, DateTime.now.plusDays(n), TimetableEntry.randomUUID)).toSet
      val timetable1 = Timetable(labwork1.id, entries, DateTime.now, Blacklist.empty, Timetable.randomUUID)
      val timetable2 = Timetable(labwork2.id, entries, DateTime.now, Blacklist.empty, Timetable.randomUUID)
      val timetable3 = Timetable(labwork3.id, entries, DateTime.now, Blacklist.empty, Timetable.randomUUID)

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
  }
}
