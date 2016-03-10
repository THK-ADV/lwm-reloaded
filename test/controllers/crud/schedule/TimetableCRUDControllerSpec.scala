package controllers.crud.schedule

import java.util.UUID

import controllers.crud.{AbstractCRUDController, AbstractCRUDControllerSpec}
import models.users.Employee
import models.{Degree, Room, AssignmentPlan, Labwork}
import models.schedule._
import models.semester.Blacklist
import org.joda.time.{LocalTime, LocalDate}
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{Json, JsValue, Writes}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import store.SesameRepository
import utils.LwmMimeType

import scala.util.{Failure, Success}

class TimetableCRUDControllerSpec extends AbstractCRUDControllerSpec[TimetableProtocol, Timetable] {

  val labworkToPass = Labwork("label to pass", "desc to pass", UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID(), AssignmentPlan.empty)
  val labworkToFail = Labwork("label to fail", "desc to fail", UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID(), AssignmentPlan.empty)

  val roomToPass = Room("room to pass", "desc to pass")
  val roomToFail = Room("room to fail", "desc to fail")

  val supervisorToPass = Employee("systemId to pass", "last name to pass", "first name to pass", "email to pass", "status to pass", Employee.randomUUID)
  val supervisorToFail = Employee("systemId to fail", "last name to fail", "first name to fail", "email to fail", "status to fail", Employee.randomUUID)

  val degreeToPass = Degree("label to pass", "abbrev to pass", Degree.randomUUID)
  val degreeToFail = Degree("label to fail", "abbrev to fail", Degree.randomUUID)

  val entriesToPass = (0 until 10).map (n =>
    TimetableEntry(
      supervisorToPass.id,
      roomToPass.id,
      degreeToPass.id,
      Weekday.toDay(n).index,
      LocalTime.now.plusHours(n),
      LocalTime.now.plusHours(n)
    )
  ).toSet
  val entriesToFail = (0 until 10).map (n =>
    TimetableEntry(
      supervisorToFail.id,
      roomToFail.id,
      degreeToFail.id,
      Weekday.toDay(n).index,
      LocalTime.now.plusHours(n),
      LocalTime.now.plusHours(n)
    )
  ).toSet

  override def entityTypeName: String = "timetable"

  override val controller: AbstractCRUDController[TimetableProtocol, Timetable] = new TimetableCRUDController(repository, namespace, roleService) {
    override protected def fromInput(input: TimetableProtocol, id: Option[UUID]): Timetable = entityToPass

    override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }

    override protected def restrictedContext(moduleId: String): PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }
  }

  override val entityToFail: Timetable = Timetable(labworkToFail.id, entriesToFail, LocalDate.now, Blacklist.empty, Timetable.randomUUID)

  override val entityToPass: Timetable = Timetable(labworkToPass.id, entriesToPass, LocalDate.now, Blacklist.empty, Timetable.randomUUID)

  override implicit val jsonWrites: Writes[Timetable] = Timetable.writes

  override val mimeType: LwmMimeType = LwmMimeType.timetableV1Json

  import ops._
  import bindings.TimetableBinding.timetableBinder
  import bindings.jodaLocalDateBinder

  override val pointedGraph: PointedGraph[Sesame] = entityToPass.toPG

  override val inputJson: JsValue = Json.obj(
    "labwork" -> entityToPass.labwork,
    "entries" -> entityToPass.entries,
    "start" -> entityToPass.start,
    "localBlacklist" -> entityToPass.localBlacklist
  )

  override val updateJson: JsValue = Json.obj(
    "labwork" -> entityToPass.labwork,
    "entries" -> entityToPass.entries,
    "start" -> entityToPass.start.plusWeeks(1),
    "localBlacklist" -> entityToPass.localBlacklist
  )

  private def toTimetableEntryAtom(entries: Set[TimetableEntry])(room: Room, supervisor: Employee, degree: Degree): Set[TimetableEntryAtom] = {
    entries.map(e => TimetableEntryAtom(supervisor, room, degree, e.dayIndex, e.start,  e.end, e.id))
  }

  val atomizedEntityToPass = TimetableAtom(
    labworkToPass,
    toTimetableEntryAtom(entriesToPass)(roomToPass, supervisorToPass, degreeToPass),
    entityToPass.start,
    entityToPass.localBlacklist,
    entityToPass.id
  )

  val atomizedEntityToFail = TimetableAtom(
    labworkToFail,
    toTimetableEntryAtom(entriesToFail)(roomToFail, supervisorToFail, degreeToFail),
    entityToFail.start,
    entityToFail.localBlacklist,
    entityToFail.id
  )

  "A TimetableCRUDControllerSpec also " should {

    s"successfully get a single $entityTypeName atomized" in {
      import Timetable.atomicWrites

      doReturn(Success(Some(entityToPass))).
      doReturn(Success(Some(labworkToPass))).
      when(repository).get(anyObject())(anyObject())

      doReturn(Success(Set(roomToPass))).
      doReturn(Success(Set(supervisorToPass))).
      doReturn(Success(Set(degreeToPass))).
      when(repository).getMany(anyObject())(anyObject())

      val request = FakeRequest(
        GET,
        s"/${entityTypeName}s/${entityToPass.id}"
      )
      val result = controller.getAtomic(entityToPass.id.toString)(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(atomizedEntityToPass)
    }

    s"not get a single $entityTypeName atomized when one of the atomic models is not found" in {
      doReturn(Success(Some(entityToPass))).
      doReturn(Success(None)).
      when(repository).get(anyObject())(anyObject())

      doReturn(Success(Set(roomToPass))).
      doReturn(Success(Set(supervisorToPass))).
      doReturn(Success(Set(degreeToPass))).
      when(repository).getMany(anyObject())(anyObject())

      val request = FakeRequest(
        GET,
        s"/${entityTypeName}s/${entityToPass.id}"
      )
      val result = controller.getAtomic(entityToPass.id.toString)(request)

      status(result) shouldBe NOT_FOUND
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "message" -> "No such element..."
      )
    }

    s"not get a single $entityTypeName atomized when there is an exception" in {
      val errorMessage = s"Oops, cant get the desired $entityTypeName for some reason"

      doReturn(Success(Some(entityToPass))).
      doReturn(Success(Some(labworkToPass))).
      when(repository).get(anyObject())(anyObject())

      doReturn(Success(Set(roomToPass))).
      doReturn(Failure(new Exception(errorMessage))).
      doReturn(Success(Set(degreeToPass))).
      when(repository).getMany(anyObject())(anyObject())

      val request = FakeRequest(
        GET,
        s"/${entityTypeName}s/${entityToPass.id}"
      )
      val result = controller.getAtomic(entityToPass.id.toString)(request)

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "errors" -> errorMessage
      )
    }

    s"successfully get all ${fgrammar(entityTypeName)} atomized" in {
      import Timetable.atomicWrites

      val timetables = Set(entityToPass, entityToFail)

      when(repository.get[Timetable](anyObject(), anyObject())).thenReturn(Success(timetables))
      doReturn(Success(Set(labworkToPass, labworkToFail))).
      doReturn(Success(Set(roomToPass))).
      doReturn(Success(Set(roomToFail))).
      doReturn(Success(Set(supervisorToPass))).
      doReturn(Success(Set(supervisorToFail))).
      doReturn(Success(Set(degreeToPass))).
      doReturn(Success(Set(degreeToFail))).
      when(repository).getMany(anyObject())(anyObject())

      val request = FakeRequest(
        GET,
        s"/${entityTypeName}s"
      )
      val result = controller.allAtomic()(request)

      status(result) shouldBe OK
      contentType(result) shouldBe Some[String](mimeType)
      contentAsJson(result) shouldBe Json.toJson(Set(atomizedEntityToPass, atomizedEntityToFail))
    }

    s"not get all ${fgrammar(entityTypeName)} atomized when there is an exception" in {
      val timetables = Set(entityToPass, entityToFail)
      val errorMessage = s"Oops, cant get the desired $entityTypeName for some reason"

      when(repository.get[Timetable](anyObject(), anyObject())).thenReturn(Success(timetables))
      doReturn(Failure(new Exception(errorMessage))).
      when(repository).getMany(anyObject())(anyObject())

      val request = FakeRequest(
        GET,
        s"/${entityTypeName}s"
      )
      val result = controller.allAtomic()(request)

      status(result) shouldBe INTERNAL_SERVER_ERROR
      contentType(result) shouldBe Some("application/json")
      contentAsJson(result) shouldBe Json.obj(
        "status" -> "KO",
        "errors" -> errorMessage
      )
    }
  }
}
