package controllers

import java.util.UUID

import base.StreamHandler._
import models._
import org.joda.time.{DateTime, LocalDate, LocalTime}
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{JsValue, Json, Writes}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import utils.LwmMimeType

import scala.util.Success

class TimetableCRUDControllerSpec extends AbstractCRUDControllerSpec[TimetableProtocol, Timetable, TimetableAtom] {

  val labworkToPass = Labwork("label to pass", "desc to pass", UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID())
  val labworkToFail = Labwork("label to fail", "desc to fail", UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID())

  val roomToPass = Room("room to pass", "desc to pass")
  val roomToFail = Room("room to fail", "desc to fail")

  val supervisorToPass = SesameEmployee("systemId to pass", "last name to pass", "first name to pass", "email to pass", "status to pass")
  val supervisorToFail = SesameEmployee("systemId to fail", "last name to fail", "first name to fail", "email to fail", "status to fail")

  val entriesToPass = (0 until 10).map(n =>
    TimetableEntry(
      Set(supervisorToPass.id),
      roomToPass.id,
      Weekday.toDay(n).index,
      LocalTime.now.plusHours(n),
      LocalTime.now.plusHours(n)
    )
  ).toSet
  val entriesToFail = (0 until 10).map(n =>
    TimetableEntry(
      Set(supervisorToFail.id),
      roomToFail.id,
      Weekday.toDay(n).index,
      LocalTime.now.plusHours(n),
      LocalTime.now.plusHours(n)
    )
  ).toSet

  override def entityTypeName: String = "timetable"

  override val controller: TimetableCRUDController = new TimetableCRUDController(repository, sessionService, namespace, roleService) {

    override protected def fromInput(input: TimetableProtocol, existing: Option[Timetable]): Timetable = entityToPass

    override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }

    override protected def restrictedContext(moduleId: String): PartialFunction[Rule, SecureContext] = {
      case _ => NonSecureBlock
    }
  }

  override val entityToFail: Timetable = Timetable(labworkToFail.id, entriesToFail, LocalDate.now, Set.empty[DateTime])

  override val entityToPass: Timetable = Timetable(labworkToPass.id, entriesToPass, LocalDate.now, Set.empty[DateTime])

  override implicit val jsonWrites: Writes[Timetable] = Timetable.writes

  override implicit def jsonWritesAtom: Writes[TimetableAtom] = Timetable.writesAtom

  override val mimeType: LwmMimeType = LwmMimeType.timetableV1Json

  import bindings.TimetableDescriptor
  import ops._

  implicit val timetableBinder = TimetableDescriptor.binder
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

  private def toTimetableEntryAtom(entries: Set[TimetableEntry])(room: Room, supervisor: SesameEmployee): Set[TimetableEntryAtom] = {
    entries.map(e => TimetableEntryAtom(Set(supervisor), room, e.dayIndex, e.start, e.end))
  }

  override val atomizedEntityToPass = TimetableAtom(
    labworkToPass,
    toTimetableEntryAtom(entriesToPass)(roomToPass, supervisorToPass),
    entityToPass.start,
    entityToPass.localBlacklist,
    entityToPass.invalidated,
    entityToPass.id
  )

  override val atomizedEntityToFail = TimetableAtom(
    labworkToFail,
    toTimetableEntryAtom(entriesToFail)(roomToFail, supervisorToFail),
    entityToFail.start,
    entityToFail.localBlacklist,
    entityToPass.invalidated,
    entityToFail.id
  )

  "A TimetableCRUDControllerSpec also " should {

    "return all timetables for a given course" in {
      val course = UUID.randomUUID
      val lab1 = Labwork("lab1", "lab1", UUID.randomUUID, course, UUID.randomUUID)
      val lab2 = Labwork("lab2", "lab2", UUID.randomUUID, course, UUID.randomUUID)

      val tt1 = Timetable(lab1.id, Set.empty[TimetableEntry], LocalDate.now, Set.empty[DateTime])
      val tt2 = Timetable(lab2.id, Set.empty[TimetableEntry], LocalDate.now, Set.empty[DateTime])
      val tt3 = Timetable(UUID.randomUUID, Set.empty[TimetableEntry], LocalDate.now, Set.empty[DateTime])
      val tt4 = Timetable(UUID.randomUUID, Set.empty[TimetableEntry], LocalDate.now, Set.empty[DateTime])
      val tt5 = Timetable(lab1.id, Set.empty[TimetableEntry], LocalDate.now, Set.empty[DateTime])
      val tt6 = Timetable(UUID.randomUUID, Set.empty[TimetableEntry], LocalDate.now, Set.empty[DateTime])
      val tt7 = Timetable(lab2.id, Set.empty[TimetableEntry], LocalDate.now, Set.empty[DateTime])
      val tt8 = Timetable(lab2.id, Set.empty[TimetableEntry], LocalDate.now, Set.empty[DateTime])

      when(repository.getAll[Timetable](anyObject())).thenReturn(Success(Set(
        tt1, tt2, tt3, tt4, tt5, tt6, tt7, tt8
      )))
      when(repository.getMany[Labwork](anyObject())(anyObject())).thenReturn(Success(Set(lab1, lab2)))

      val request = FakeRequest(
        GET,
        s"/$entityTypeName?${TimetableCRUDController.courseAttribute}=$course"
      )
      val result = controller.all()(request)
      val expected = Set(Json.toJson(tt1), Json.toJson(tt2), Json.toJson(tt5), Json.toJson(tt7), Json.toJson(tt8))

      status(result) shouldBe OK
      contentType(result) shouldBe Some(mimeType.value)
      contentFromStream(result) shouldBe expected
    }

    "return all timetables for a given labwork" in {
      val lab1 = Labwork("lab1", "lab2", UUID.randomUUID, UUID.randomUUID, UUID.randomUUID)
      val lab2 = Labwork("lab2", "lab2", UUID.randomUUID, UUID.randomUUID, UUID.randomUUID)
      val lab3 = Labwork("lab3", "lab3", UUID.randomUUID, UUID.randomUUID, UUID.randomUUID)
      val lab4 = Labwork("lab4", "lab4", UUID.randomUUID, UUID.randomUUID, UUID.randomUUID)

      val tt1 = Timetable(lab1.id, Set.empty[TimetableEntry], LocalDate.now, Set.empty[DateTime])
      val tt2 = Timetable(lab2.id, Set.empty[TimetableEntry], LocalDate.now, Set.empty[DateTime])
      val tt3 = Timetable(UUID.randomUUID, Set.empty[TimetableEntry], LocalDate.now, Set.empty[DateTime])
      val tt4 = Timetable(UUID.randomUUID, Set.empty[TimetableEntry], LocalDate.now, Set.empty[DateTime])
      val tt5 = Timetable(lab3.id, Set.empty[TimetableEntry], LocalDate.now, Set.empty[DateTime])
      val tt6 = Timetable(UUID.randomUUID, Set.empty[TimetableEntry], LocalDate.now, Set.empty[DateTime])
      val tt7 = Timetable(lab4.id, Set.empty[TimetableEntry], LocalDate.now, Set.empty[DateTime])
      val tt8 = Timetable(UUID.randomUUID, Set.empty[TimetableEntry], LocalDate.now, Set.empty[DateTime])

      when(repository.getAll[Timetable](anyObject())).thenReturn(Success(Set(
        tt1, tt2, tt3, tt4, tt5, tt6, tt7, tt8
      )))

      val request = FakeRequest(
        GET,
        s"/$entityTypeName?${TimetableCRUDController.labworkAttribute}=${lab3.id}"
      )
      val result = controller.all()(request)
      val expected = Set(Json.toJson(tt5))

      status(result) shouldBe OK
      contentType(result) shouldBe Some(mimeType.value)
      contentFromStream(result) shouldBe expected
    }
  }
}
